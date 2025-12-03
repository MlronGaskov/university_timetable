package ru.nsu.university.timetable.user.student.dto;

import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.UUID;

public record StudentResponse(
        UUID id,
        String fullName,
        String studentId,
        Status status,
        Instant createdAt,
        Instant updatedAt
) {
}
