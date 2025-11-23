package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.Status;
import ru.nsu.university.timetable.domain.Teacher;
import ru.nsu.university.timetable.dto.teachers.CreateTeacherRequest;
import ru.nsu.university.timetable.dto.teachers.TeacherResponse;
import ru.nsu.university.timetable.dto.teachers.UpdateTeacherRequest;
import ru.nsu.university.timetable.repo.TeacherRepository;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class TeacherService {
    private final TeacherRepository repo;

    public TeacherResponse create(CreateTeacherRequest req) {
        if (repo.existsByFullNameIgnoreCase(req.fullName()))
            throw new IllegalArgumentException("teacher already exists");

        Teacher t = Teacher.builder()
                .fullName(req.fullName())
                .status(Status.ACTIVE)
                .build();

        return map(repo.save(t));
    }

    public List<TeacherResponse> findAll() {
        return repo.findAll().stream().map(this::map).toList();
    }

    public TeacherResponse findOne(UUID id) {
        return repo.findById(id).map(this::map).orElseThrow();
    }

    public TeacherResponse update(UUID id, UpdateTeacherRequest req) {
        Teacher t = repo.findById(id).orElseThrow();
        if (req.fullName() != null && !req.fullName().equalsIgnoreCase(t.getFullName())) {
            if (repo.existsByFullNameIgnoreCase(req.fullName()))
                throw new IllegalArgumentException("teacher already exists");
            t.setFullName(req.fullName());
        }
        return map(t);
    }

    public TeacherResponse archive(UUID id) {
        Teacher t = repo.findById(id).orElseThrow();
        t.setStatus(Status.INACTIVE);
        return map(t);
    }

    private TeacherResponse map(Teacher t) {
        return new TeacherResponse(
                t.getId(),
                t.getFullName(),
                t.getStatus(),
                t.getCreatedAt(),
                t.getUpdatedAt()
        );
    }
}
