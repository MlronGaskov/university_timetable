package ru.nsu.university.timetable.schedule.course;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.schedule.course.dto.CourseResponse;
import ru.nsu.university.timetable.schedule.course.dto.CreateCourseRequest;
import ru.nsu.university.timetable.schedule.course.dto.UpdateCourseRequest;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/courses")
@RequiredArgsConstructor
public class CourseController {
    private final CourseService service;

    @GetMapping
    public List<CourseResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public CourseResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public CourseResponse create(@RequestBody @Validated CreateCourseRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public CourseResponse update(@PathVariable UUID id,
                                 @RequestBody @Validated UpdateCourseRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public CourseResponse archive(@PathVariable UUID id) {
        return service.archive(id);
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasRole('ADMIN')")
    public CourseResponse activate(@PathVariable UUID id) {
        return service.activate(id);
    }

    @PostMapping("/{courseId}/groups/{groupCode}")
    @PreAuthorize("hasRole('ADMIN')")
    public CourseResponse addGroup(@PathVariable UUID courseId,
                                   @PathVariable String groupCode) {
        return service.addGroup(courseId, groupCode);
    }

    @DeleteMapping("/{courseId}/groups/{groupCode}")
    @PreAuthorize("hasRole('ADMIN')")
    public CourseResponse removeGroup(@PathVariable UUID courseId,
                                      @PathVariable String groupCode) {
        return service.removeGroup(courseId, groupCode);
    }
}
