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
            String code,
            Instant startAt,
            Instant endAt,
            String policyName
    ) {
    }

    public record CourseDto(
            String code,
            String teacherId,
            List<String> groupCodes,
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
            String day,
            String startTime,
            String endTime
    ) {
    }

    public record PolicyDto(
            String name,
            String gridJson,
            String breaksJson,
            String limitsJson,
            String travelMatrixJson,
            String weightsJson
    ) {
    }
}
