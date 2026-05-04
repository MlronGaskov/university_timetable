package ru.nsu.university.timetable.solver.dto;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.time.Instant;
import java.util.List;

@JsonInclude(JsonInclude.Include.NON_NULL)
public record SolverRequest(
        SemesterDto semester,
        List<RoomDto> rooms,
        List<TeacherDto> teachers,
        PolicyDto policy,
        List<FixedSlotDto> fixedSlots,
        List<SlotToPlaceDto> slotsToPlace
) {
    public record SemesterDto(
            String code,
            Instant startAt,
            Instant endAt
    ) {
        public SemesterDto(String id, Instant startAt, Instant endAt, String policyId) {
            this(id, startAt, endAt);
        }
    }

    public record CourseDto(
            String id,
            String teacherId,
            List<String> groupIds,
            int plannedHours,
            int requiredRoomCapacity,
            List<EquipmentRequirementDto> equipmentRequirements
    ) {
    }

    public record RoomDto(
            String roomCode,
            int capacity,
            List<RoomItemDto> items
    ) {
    }

    public record RoomItemDto(
            String name,
            int quantity
    ) {
    }

    public record TeacherDto(
            String teacherId,
            List<WorkingIntervalDto> preferredWorkingHours
    ) {
    }

    public record WorkingIntervalDto(
            String day,
            String startTime,
            String endTime
    ) {
    }

    public record EquipmentRequirementDto(
            String name,
            int quantity
    ) {
    }

    public record PolicyDto(
            String id,
            String gridJson,
            String breaksJson,
            String limitsJson,
            String travelMatrixJson,
            String weightsJson,
            String searchJson
    ) {
        public PolicyDto(
                String id,
                String gridJson,
                String breaksJson,
                String limitsJson,
                String travelMatrixJson,
                String weightsJson
        ) {
            this(id, gridJson, breaksJson, limitsJson, travelMatrixJson, weightsJson, null);
        }
    }

    public record FixedSlotDto(
            String slotId,
            String courseCode,
            String teacherId,
            List<String> groupIds,
            String roomCode,
            String dayOfWeek,
            String startTime,
            String endTime
    ) {
    }

    public record SlotToPlaceDto(
            String requestId,
            String courseCode,
            String teacherId,
            List<String> groupIds,
            int requiredRoomCapacity,
            List<EquipmentRequirementDto> equipmentRequirements
    ) {
    }

    public static <T> List<T> copyOrEmpty(List<T> value) {
        return value == null ? List.of() : List.copyOf(value);
    }
}
