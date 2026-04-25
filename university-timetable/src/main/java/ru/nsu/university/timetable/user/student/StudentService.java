package ru.nsu.university.timetable.user.student;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.student.dto.CreateStudentRequest;
import ru.nsu.university.timetable.user.student.dto.StudentResponse;
import ru.nsu.university.timetable.user.student.dto.UpdateStudentRequest;
import ru.nsu.university.timetable.web.OptimisticLockingGuard;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class StudentService {
    private final StudentRepository repository;

    public StudentResponse create(CreateStudentRequest req) {
        ensureStudentIdUnique(req.studentId(), null);

        Student entity = Student.builder()
                .fullName(normalizeName(req.fullName()))
                .studentId(req.studentId().trim())
                .status(Status.ACTIVE)
                .build();

        return toDto(repository.saveAndFlush(entity));
    }

    @Transactional(readOnly = true)
    public List<StudentResponse> findAll() {
        return repository.findAll()
                .stream()
                .map(this::toDto)
                .toList();
    }

    @Transactional(readOnly = true)
    public StudentResponse findOne(UUID id) {
        Student s = repository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Student not found: " + id));
        return toDto(s);
    }

    public StudentResponse update(UUID id, long expectedVersion, UpdateStudentRequest req) {
        Student s = findEntityForMutation(id, expectedVersion);

        if (req.fullName() != null) {
            s.setFullName(normalizeName(req.fullName()));
        }

        if (req.studentId() != null && !req.studentId().equals(s.getStudentId())) {
            ensureStudentIdUnique(req.studentId(), id);
            s.setStudentId(req.studentId().trim());
        }

        return toDto(repository.saveAndFlush(s));
    }

    public StudentResponse archive(UUID id, long expectedVersion) {
        Student s = findEntityForMutation(id, expectedVersion);
        s.setStatus(Status.INACTIVE);
        return toDto(repository.saveAndFlush(s));
    }

    public StudentResponse activate(UUID id, long expectedVersion) {
        Student s = findEntityForMutation(id, expectedVersion);
        s.setStatus(Status.ACTIVE);
        return toDto(repository.saveAndFlush(s));
    }

    private Student findEntityForMutation(UUID id, long expectedVersion) {
        Student s = repository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Student not found: " + id));
        OptimisticLockingGuard.verifyVersion("Student", id, expectedVersion, s.getVersion());
        return s;
    }

    private void ensureStudentIdUnique(String studentId, UUID currentId) {
        String trimmed = studentId == null ? null : studentId.trim();
        if (trimmed == null || trimmed.isEmpty()) {
            throw new IllegalArgumentException("studentId must not be blank");
        }

        boolean exists = repository.existsByStudentId(trimmed);

        if (exists && currentId == null) {
            throw new IllegalArgumentException("studentId already exists");
        }
        if (exists) {
            repository.findByStudentId(trimmed).ifPresent(other -> {
                if (!other.getId().equals(currentId)) {
                    throw new IllegalArgumentException("studentId already exists");
                }
            });
        }
    }

    private String normalizeName(String name) {
        if (name == null) return null;
        return name.trim();
    }

    private StudentResponse toDto(Student s) {
        return new StudentResponse(
                s.getId(),
                s.getVersion(),
                s.getFullName(),
                s.getStudentId(),
                s.getStatus(),
                s.getCreatedAt(),
                s.getUpdatedAt()
        );
    }
}
