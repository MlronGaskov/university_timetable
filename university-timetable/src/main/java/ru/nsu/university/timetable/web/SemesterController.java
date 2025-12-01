package ru.nsu.university.timetable.web;

import java.util.List;
import java.util.UUID;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;

import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import ru.nsu.university.timetable.domain.SemesterStatus;
import ru.nsu.university.timetable.dto.semesters.CreateSemesterRequest;
import ru.nsu.university.timetable.dto.semesters.SemesterResponse;
import ru.nsu.university.timetable.dto.semesters.UpdateSemesterRequest;
import ru.nsu.university.timetable.service.SemesterService;

@RestController
@RequestMapping("/api/semesters")
@RequiredArgsConstructor
public class SemesterController {

    private final SemesterService semesterService;

    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN')")
    public SemesterResponse create(@Valid @RequestBody CreateSemesterRequest request) {
        return semesterService.create(request);
    }

    @GetMapping("/{id}")
    @PreAuthorize("isAuthenticated()")
    public SemesterResponse getById(@PathVariable UUID id) {
        return semesterService.getById(id);
    }

    @GetMapping
    @PreAuthorize("isAuthenticated()")
    public List<SemesterResponse> list(@RequestParam(required = false) SemesterStatus status) {
        return semesterService.list(status);
    }

    @PutMapping("/{id}/update")
    @PreAuthorize("hasRole('ADMIN')")
    public SemesterResponse update(@PathVariable UUID id,
                              @Valid @RequestBody UpdateSemesterRequest request) {
        return semesterService.update(id, request);
    }

    @DeleteMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public void archiveOrDelete(@PathVariable UUID id,
                                @RequestParam(name = "hard", defaultValue = "false") boolean hardDelete) {
        semesterService.archiveOrDelete(id, hardDelete);
    }
}

