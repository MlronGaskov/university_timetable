package ru.nsu.university.timetable.user.auth.dto;

import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.auth.Role;

import java.time.Instant;

public record UserResponse(
        java.util.UUID id,
        String login,
        String email,
        Status status,
        Role role,
        String teacherId,
        String teacherName,
        String studentId,
        String studentName,
        Instant createdAt,
        Instant updatedAt
) {
}
