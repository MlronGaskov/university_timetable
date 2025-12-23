package ru.nsu.university.timetable.solver.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import ru.nsu.university.timetable.schedule.timetable.WeekPattern;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public record SolverResponse(
        List<SlotDto> slots,
        List<UnplacedDto> unplaced,
        Double evaluationScore,
        Double evaluationPenalty
) {
    @JsonIgnoreProperties(ignoreUnknown = true)
    public record SlotDto(
            String courseId,
            String roomCode,
            String dayOfWeek,
            String startTime,
            String endTime,
            String validFrom,
            String validUntil,
            WeekPattern weekPattern
    ) {}

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record UnplacedDto(
            String courseId,
            String reason
    ) {}
}
