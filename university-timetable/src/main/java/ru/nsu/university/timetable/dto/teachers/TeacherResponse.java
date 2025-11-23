package ru.nsu.university.timetable.dto.teachers;

import ru.nsu.university.timetable.domain.Status;

import java.time.Instant;
import java.util.UUID;

public record TeacherResponse(
        UUID id,
        String fullName,
        Status status,
        Instant createdAt,
        Instant updatedAt
) {}
