package ru.nsu.university.timetable.solver.dto;

import java.time.Instant;
import java.util.List;

public record SolverRequest(
        SemesterDto semester,
        List<CourseDto> courses,
        List<RoomDto> rooms,
        List<TeacherDto> teachers,
        PolicyDto policy
) {
    public record SemesterDto(
            String id,
            Instant startAt,
            Instant endAt,
            String policyId
    ) {
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
            String weightsJson
    ) {
    }
}
