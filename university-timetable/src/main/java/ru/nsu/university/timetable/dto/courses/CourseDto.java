package ru.nsu.university.timetable.dto.courses;

import ru.nsu.university.timetable.domain.ActivityForm;
import ru.nsu.university.timetable.domain.CourseStatus;

import java.time.Instant;
import java.util.Set;
import java.util.UUID;

public record CourseDto(
        UUID id,
        String code,
        String title,
        String department,
        Set<ActivityForm> activityForms,
        int lectureHours,
        int seminarHours,
        int labHours,
        String roomRequirements,
        Set<UUID> equipmentRequirementIds,
        CourseStatus status,
        Instant createdAt,
        Instant updatedAt
) {
}
