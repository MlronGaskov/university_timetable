package ru.nsu.university.timetable.catalog.group;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.catalog.group.dto.CreateGroupRequest;
import ru.nsu.university.timetable.catalog.group.dto.GroupResponse;
import ru.nsu.university.timetable.catalog.group.dto.UpdateGroupRequest;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/groups")
@RequiredArgsConstructor
public class GroupController {
    private final GroupService service;

    @GetMapping
    public List<GroupResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public GroupResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse create(@RequestBody @Validated CreateGroupRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse update(@PathVariable UUID id,
                                @RequestBody @Validated UpdateGroupRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse archive(@PathVariable UUID id) {
        return service.archive(id);
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse activate(@PathVariable UUID id) {
        return service.activate(id);
    }

    @PostMapping("/{groupId}/students/{studentId}")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse addStudent(@PathVariable UUID groupId,
                                    @PathVariable UUID studentId) {
        return service.addStudent(groupId, studentId);
    }

    @DeleteMapping("/{groupId}/students/{studentId}")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse removeStudent(@PathVariable UUID groupId,
                                       @PathVariable UUID studentId) {
        return service.removeStudent(groupId, studentId);
    }
}
