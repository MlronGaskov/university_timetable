package ru.nsu.university.timetable.web;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.domain.ActivityForm;
import ru.nsu.university.timetable.dto.courses.CourseDto;
import ru.nsu.university.timetable.dto.courses.CourseListItemDto;
import ru.nsu.university.timetable.dto.courses.CreateCourseRequest;
import ru.nsu.university.timetable.dto.courses.UpdateCourseRequest;
import ru.nsu.university.timetable.service.CourseService;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/courses")
@RequiredArgsConstructor
public class CourseController {

    private final CourseService courseService;

    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public CourseDto create(@Valid @RequestBody CreateCourseRequest request) {
        return courseService.create(request);
    }

    @GetMapping("/{id}")
    @PreAuthorize("isAuthenticated()")
    public CourseDto getById(@PathVariable UUID id) {
        return courseService.getById(id);
    }

    @GetMapping
    @PreAuthorize("isAuthenticated()")
    public List<CourseListItemDto> list(@RequestParam(required = false) String department,
                                        @RequestParam(required = false) ActivityForm activityForm) {
        return courseService.list(department, activityForm);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public CourseDto update(@PathVariable UUID id,
                            @Valid @RequestBody UpdateCourseRequest request) {
        return courseService.update(id, request);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public void archiveOrDelete(@PathVariable UUID id,
                                @RequestParam(name = "hard", defaultValue = "false") boolean hardDelete) {
        courseService.archiveOrDelete(id, hardDelete);
    }
}
