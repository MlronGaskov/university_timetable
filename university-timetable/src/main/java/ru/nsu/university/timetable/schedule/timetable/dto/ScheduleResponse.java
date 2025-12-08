package ru.nsu.university.timetable.schedule.timetable.dto;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record ScheduleResponse(
        UUID id,
        UUID semesterId,
        int version,
        Double evaluationScore,
        Instant createdAt,
        Instant updatedAt,
        List<ScheduleSlotDto> slots
) {
}
