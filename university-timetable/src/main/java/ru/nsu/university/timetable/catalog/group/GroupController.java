package ru.nsu.university.timetable.catalog.group;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.catalog.group.dto.CreateGroupRequest;
import ru.nsu.university.timetable.catalog.group.dto.GroupResponse;
import ru.nsu.university.timetable.catalog.group.dto.UpdateGroupRequest;
import ru.nsu.university.timetable.web.IfMatchVersion;

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
                                @RequestHeader(name = "If-Match", required = false) String ifMatch,
                                @RequestBody @Validated UpdateGroupRequest req) {
        return service.update(id, IfMatchVersion.parseRequired(ifMatch), req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse archive(@PathVariable UUID id,
                                 @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.archive(id, IfMatchVersion.parseRequired(ifMatch));
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse activate(@PathVariable UUID id,
                                  @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.activate(id, IfMatchVersion.parseRequired(ifMatch));
    }

    @PostMapping("/{groupId}/students/{studentId}")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse addStudent(@PathVariable UUID groupId,
                                    @PathVariable String studentId,
                                    @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.addStudent(groupId, IfMatchVersion.parseRequired(ifMatch), studentId);
    }

    @DeleteMapping("/{groupId}/students/{studentId}")
    @PreAuthorize("hasRole('ADMIN')")
    public GroupResponse removeStudent(@PathVariable UUID groupId,
                                       @PathVariable String studentId,
                                       @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.removeStudent(groupId, IfMatchVersion.parseRequired(ifMatch), studentId);
    }
}
