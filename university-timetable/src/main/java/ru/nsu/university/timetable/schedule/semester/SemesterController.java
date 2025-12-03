package ru.nsu.university.timetable.schedule.semester;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.schedule.semester.dto.CreateSemesterRequest;
import ru.nsu.university.timetable.schedule.semester.dto.SemesterResponse;
import ru.nsu.university.timetable.schedule.semester.dto.UpdateSemesterRequest;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/semesters")
@RequiredArgsConstructor
public class SemesterController {
    private final SemesterService service;

    @GetMapping
    public List<SemesterResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public SemesterResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public SemesterResponse create(@RequestBody @Validated CreateSemesterRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public SemesterResponse update(@PathVariable UUID id,
                                   @RequestBody @Validated UpdateSemesterRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public SemesterResponse archive(@PathVariable UUID id) {
        return service.archive(id);
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasRole('ADMIN')")
    public SemesterResponse activate(@PathVariable UUID id) {
        return service.activate(id);
    }
}
