package ru.nsu.university.timetable.user.teacher.dto;

import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record TeacherResponse(
        UUID id,
        String teacherId,
        String fullName,
        Status status,
        List<WorkingIntervalDto> preferredWorkingHours,
        Instant createdAt,
        Instant updatedAt
) {
}
