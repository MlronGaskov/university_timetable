package ru.nsu.university.timetable.schedule.course.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Size;

import java.util.List;
import java.util.UUID;

public record UpdateCourseRequest(
        @Size(max = 64)
        String code,

        @Size(max = 256)
        String title,

        @Size(max = 64)
        String teacherId,

        @Min(0)
        Integer plannedHours,

        @Valid
        List<CourseEquipmentItemDto> equipmentRequirements,

        List<UUID> groupIds
) {
}
