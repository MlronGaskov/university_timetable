package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.groups.SubgroupDto;
import ru.nsu.university.timetable.dto.groups.UpdateSubgroupRequest;
import ru.nsu.university.timetable.service.GroupService;

import java.util.UUID;

@RestController
@RequestMapping("/api/subgroups")
@RequiredArgsConstructor
public class SubgroupController {
    private final GroupService service;

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public SubgroupDto update(@PathVariable UUID id,
                              @RequestBody @Validated UpdateSubgroupRequest req) {
        return service.updateSubgroup(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public SubgroupDto archive(@PathVariable UUID id) {
        return service.archiveSubgroup(id);
    }
}
