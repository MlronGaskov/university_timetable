package ru.nsu.university.timetable.user.student;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.user.student.dto.CreateStudentRequest;
import ru.nsu.university.timetable.user.student.dto.StudentResponse;
import ru.nsu.university.timetable.user.student.dto.UpdateStudentRequest;

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
    public StudentResponse create(@RequestBody @Valid CreateStudentRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public StudentResponse update(@PathVariable UUID id,
                                  @RequestBody @Valid UpdateStudentRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public StudentResponse archive(@PathVariable UUID id) {
        return service.archive(id);
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasRole('ADMIN')")
    public StudentResponse activate(@PathVariable UUID id) {
        return service.activate(id);
    }
}
