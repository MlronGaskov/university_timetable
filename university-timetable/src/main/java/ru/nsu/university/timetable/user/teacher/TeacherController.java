package ru.nsu.university.timetable.user.teacher;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.user.teacher.dto.CreateTeacherRequest;
import ru.nsu.university.timetable.user.teacher.dto.TeacherResponse;
import ru.nsu.university.timetable.user.teacher.dto.UpdateTeacherRequest;
import ru.nsu.university.timetable.user.teacher.dto.UpdateWorkingHoursRequest;
import ru.nsu.university.timetable.web.IfMatchVersion;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/teachers")
@RequiredArgsConstructor
public class TeacherController {
    private final TeacherService service;

    @GetMapping
    public List<TeacherResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public TeacherResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public TeacherResponse create(@RequestBody @Validated CreateTeacherRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public TeacherResponse update(@PathVariable UUID id,
                                  @RequestHeader(name = "If-Match", required = false) String ifMatch,
                                  @RequestBody @Validated UpdateTeacherRequest req) {
        return service.update(id, IfMatchVersion.parseRequired(ifMatch), req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public TeacherResponse archive(@PathVariable UUID id,
                                   @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.archive(id, IfMatchVersion.parseRequired(ifMatch));
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasRole('ADMIN')")
    public TeacherResponse activate(@PathVariable UUID id,
                                    @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.activate(id, IfMatchVersion.parseRequired(ifMatch));
    }

    @PutMapping("/{id}/working-hours")
    @PreAuthorize("@securityService.canUpdateTeacherWorkingHours(#id)")
    public TeacherResponse updateWorkingHours(@PathVariable UUID id,
                                              @RequestHeader(name = "If-Match", required = false) String ifMatch,
                                              @RequestBody @Valid UpdateWorkingHoursRequest req) {
        return service.updateWorkingHours(id, IfMatchVersion.parseRequired(ifMatch), req);
    }
}
