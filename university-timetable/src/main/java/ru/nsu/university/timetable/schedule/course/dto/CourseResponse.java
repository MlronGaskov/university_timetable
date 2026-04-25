package ru.nsu.university.timetable.schedule.course.dto;

import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record CourseResponse(
        UUID id,
        long version,
        String code,
        String title,
        Status status,
        int plannedHours,
        int requiredRoomCapacity,
        String teacherId,
        List<String> groupCodes,
        List<CourseEquipmentItemDto> equipmentRequirements,
        Instant createdAt,
        Instant updatedAt
) {
}
