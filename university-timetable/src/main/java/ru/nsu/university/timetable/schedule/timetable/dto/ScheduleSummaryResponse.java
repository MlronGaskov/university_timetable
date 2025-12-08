package ru.nsu.university.timetable.schedule.timetable.dto;

import java.time.Instant;
import java.util.UUID;

public record ScheduleSummaryResponse(
        UUID id,
        String semesterCode,
        int version,
        Double evaluationScore,
        Instant createdAt,
        Instant updatedAt
) {
}
