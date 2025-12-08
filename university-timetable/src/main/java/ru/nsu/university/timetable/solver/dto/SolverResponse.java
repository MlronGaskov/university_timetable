package ru.nsu.university.timetable.solver.dto;

import ru.nsu.university.timetable.schedule.timetable.WeekPattern;

import java.util.List;
import java.util.UUID;

public record SolverResponse(
        List<SlotDto> slots,
        Double evaluationScore
) {
    public record SlotDto(
            UUID courseId,
            String roomCode,
            String dayOfWeek,
            String startTime,
            String endTime,
            String validFrom,
            String validUntil,
            WeekPattern weekPattern
    ) { }
}
