package ru.nsu.university.timetable.solver.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public record SolverErrorResponse(
        String error,
        String message,
        List<FixedSlotConflictDto> conflicts
) {
    public SolverErrorResponse {
        conflicts = SolverRequest.copyOrEmpty(conflicts);
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record FixedSlotConflictDto(
            String type,
            List<String> slotIds,
            String roomCode,
            String teacherId,
            String groupId,
            String dayOfWeek,
            String startTime,
            String endTime
    ) {
        public FixedSlotConflictDto {
            slotIds = SolverRequest.copyOrEmpty(slotIds);
        }
    }
}
