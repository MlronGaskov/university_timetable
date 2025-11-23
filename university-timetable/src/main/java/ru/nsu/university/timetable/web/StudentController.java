package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.students.CreateStudentRequest;
import ru.nsu.university.timetable.dto.students.StudentResponse;
import ru.nsu.university.timetable.dto.students.UpdateStudentRequest;
import ru.nsu.university.timetable.service.StudentService;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/students")
@RequiredArgsConstructor
public class StudentController {
    private final StudentService service;

    @GetMapping
    public List<StudentResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public StudentResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public StudentResponse create(@RequestBody @Validated CreateStudentRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public StudentResponse update(@PathVariable UUID id,
                                  @RequestBody @Validated UpdateStudentRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public StudentResponse archive(@PathVariable UUID id) {
        return service.archive(id);
    }
}
