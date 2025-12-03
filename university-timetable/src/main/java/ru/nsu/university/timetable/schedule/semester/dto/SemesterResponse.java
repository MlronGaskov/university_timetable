package ru.nsu.university.timetable.schedule.semester.dto;

import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record SemesterResponse(
        UUID id,
        String code,
        Instant startAt,
        Instant endAt,
        Status status,
        UUID policyId,
        List<UUID> courseIds,
        List<String> roomCodes,
        Instant createdAt,
        Instant updatedAt
) {
}
