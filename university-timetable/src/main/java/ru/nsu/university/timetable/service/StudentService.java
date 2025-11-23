package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.Group;
import ru.nsu.university.timetable.domain.Status;
import ru.nsu.university.timetable.domain.Student;
import ru.nsu.university.timetable.dto.students.CreateStudentRequest;
import ru.nsu.university.timetable.dto.students.StudentResponse;
import ru.nsu.university.timetable.dto.students.UpdateStudentRequest;
import ru.nsu.university.timetable.repo.GroupRepository;
import ru.nsu.university.timetable.repo.StudentRepository;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class StudentService {
    private final StudentRepository repo;
    private final GroupRepository groupRepo;

    public StudentResponse create(CreateStudentRequest req) {
        if (repo.existsByStudentId(req.studentId()))
            throw new IllegalArgumentException("studentId already exists");

        Group group = null;
        if (req.groupId() != null) {
            group = groupRepo.findById(req.groupId()).orElseThrow();
        }

        Student s = Student.builder()
                .fullName(req.fullName())
                .studentId(req.studentId())
                .group(group)
                .status(Status.ACTIVE)
                .build();

        return map(repo.save(s));
    }

    public List<StudentResponse> findAll() {
        return repo.findAll().stream().map(this::map).toList();
    }

    public StudentResponse findOne(UUID id) {
        return repo.findById(id).map(this::map).orElseThrow();
    }

    public StudentResponse update(UUID id, UpdateStudentRequest req) {
        Student s = repo.findById(id).orElseThrow();

        if (req.fullName() != null) s.setFullName(req.fullName());

        if (req.studentId() != null && !req.studentId().equals(s.getStudentId())) {
            if (repo.existsByStudentId(req.studentId()))
                throw new IllegalArgumentException("studentId already exists");
            s.setStudentId(req.studentId());
        }

        if (req.groupId() != null) {
            Group g = groupRepo.findById(req.groupId()).orElseThrow();
            s.setGroup(g);
        }

        return map(s);
    }

    public StudentResponse archive(UUID id) {
        Student s = repo.findById(id).orElseThrow();
        s.setStatus(Status.INACTIVE);
        return map(s);
    }

    private StudentResponse map(Student s) {
        UUID groupId = s.getGroup() != null ? s.getGroup().getId() : null;
        String groupName = s.getGroup() != null ? s.getGroup().getName() : null;

        return new StudentResponse(
                s.getId(),
                s.getFullName(),
                s.getStudentId(),
                groupId,
                groupName,
                s.getStatus(),
                s.getCreatedAt(),
                s.getUpdatedAt()
        );
    }
}
