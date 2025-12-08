package ru.nsu.university.timetable.schedule.timetable.dto;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record ScheduleResponse(
        UUID id,
        String semesterCode,
        int version,
        Double evaluationScore,
        Instant createdAt,
        Instant updatedAt,
        List<ScheduleSlotDto> slots
) {
}
