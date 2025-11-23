package ru.nsu.university.timetable.dto.students;

import ru.nsu.university.timetable.domain.Status;

import java.time.Instant;
import java.util.UUID;

public record StudentResponse(
        UUID id,
        String fullName,
        String studentId,
        UUID groupId,
        String groupName,
        Status status,
        Instant createdAt,
        Instant updatedAt
) {}
