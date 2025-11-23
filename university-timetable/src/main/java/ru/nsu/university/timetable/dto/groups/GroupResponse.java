package ru.nsu.university.timetable.dto.groups;

import ru.nsu.university.timetable.domain.Status;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record GroupResponse(
        UUID id,
        String name,
        String code,
        int size,
        Status status,
        Instant createdAt,
        Instant updatedAt,
        List<SubgroupDto> subgroups
) {}
