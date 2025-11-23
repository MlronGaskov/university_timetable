package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.groups.*;
import ru.nsu.university.timetable.service.GroupService;

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

    @PostMapping("/{id}/subgroups")
    @PreAuthorize("hasRole('ADMIN')")
    public SubgroupDto createSubgroup(@PathVariable UUID id,
                                      @RequestBody @Validated CreateSubgroupRequest req) {
        return service.createSubgroup(id, req);
    }
}
