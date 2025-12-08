package ru.nsu.university.timetable.solver.dto;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record SolverRequest(
        SemesterDto semester,
        List<CourseDto> courses,
        List<RoomDto> rooms,
        List<TeacherDto> teachers,
        PolicyDto policy
) {
    public record SemesterDto(
            UUID id,
            Instant startAt,
            Instant endAt,
            UUID policyId
    ) {
    }

    public record CourseDto(
            UUID id,
            String teacherId,
            List<UUID> groupIds,
            int plannedHours,
            int requiredRoomCapacity
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
            String day,       // "MONDAY"
            String startTime, // "09:00"
            String endTime    // "10:30"
    ) {
    }

    public record PolicyDto(
            UUID id,
            String gridJson,
            String breaksJson,
            String limitsJson,
            String travelMatrixJson,
            String weightsJson
    ) {
    }
}
