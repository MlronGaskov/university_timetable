package ru.nsu.university.timetable.schedule.course.dto;

import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record CourseResponse(
        UUID id,
        String code,
        String title,
        Status status,
        int plannedHours,
        int requiredRoomCapacity,
        String teacherId,
        List<UUID> groupIds,
        List<CourseEquipmentItemDto> equipmentRequirements,
        Instant createdAt,
        Instant updatedAt
) {
}
