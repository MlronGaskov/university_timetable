package ru.nsu.university.timetable.dto.semesters;

import java.time.Instant;
import java.util.UUID;

import ru.nsu.university.timetable.domain.SemesterStatus;

public record SemesterResponse(
        UUID id,
        String code,
        Instant start,
        Instant end,
        SemesterStatus status,
        Instant createdAt,
        Instant updatedAt
) {
}

