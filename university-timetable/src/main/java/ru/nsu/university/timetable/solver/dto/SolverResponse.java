package ru.nsu.university.timetable.solver.dto;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import ru.nsu.university.timetable.schedule.timetable.WeekPattern;

import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
public record SolverResponse(
        List<SlotDto> placedSlots,
        List<SlotDto> fixedSlots,
        List<SlotDto> scheduleSlots,
        List<UnplacedDto> unplaced,
        WarningsDto warnings,
        Double evaluationScore,
        Double evaluationPenalty
) {
    public List<SlotDto> slots() {
        return placedSlotsOrEmpty();
    }

    public List<SlotDto> placedSlotsOrEmpty() {
        return placedSlots == null ? List.of() : placedSlots;
    }

    public List<SlotDto> fixedSlotsOrEmpty() {
        return fixedSlots == null ? List.of() : fixedSlots;
    }

    public List<SlotDto> scheduleSlotsOrEmpty() {
        return scheduleSlots == null ? List.of() : scheduleSlots;
    }

    public List<UnplacedDto> unplacedOrEmpty() {
        return unplaced == null ? List.of() : unplaced;
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record SlotDto(
            @JsonAlias({"courseId", "courseCode"}) String courseCode,
            String roomCode,
            String dayOfWeek,
            String startTime,
            String endTime,
            String validFrom,
            String validUntil,
            WeekPattern weekPattern,
            String requestId,
            String slotId,
            String source
    ) {
        public String courseId() {
            return courseCode;
        }
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record UnplacedDto(
            @JsonAlias({"courseId", "courseCode"}) String courseCode,
            String requestId,
            String reason
    ) {
        public String courseId() {
            return courseCode;
        }
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record WarningsDto(
            List<FixedSlotWarningDto> fixedSlots
    ) {
        public List<FixedSlotWarningDto> fixedSlotsOrEmpty() {
            return fixedSlots == null ? List.of() : fixedSlots;
        }
    }

    @JsonIgnoreProperties(ignoreUnknown = true)
    public record FixedSlotWarningDto(
            String slotId,
            String type,
            String message
    ) {
    }
}
