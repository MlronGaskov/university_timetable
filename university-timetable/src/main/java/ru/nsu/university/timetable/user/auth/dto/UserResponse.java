package ru.nsu.university.timetable.user.auth.dto;

import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.auth.Role;

import java.time.Instant;
import java.util.UUID;

public record UserResponse(
        UUID id,
        String login,
        String email,
        Status status,
        Role role,
        UUID teacherId,
        String teacherName,
        UUID studentId,
        String studentName,
        Instant createdAt,
        Instant updatedAt
) {
}
