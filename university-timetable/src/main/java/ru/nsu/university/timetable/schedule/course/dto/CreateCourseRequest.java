package ru.nsu.university.timetable.schedule.course.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.util.List;
import java.util.UUID;

public record CreateCourseRequest(
        @NotBlank
        @Size(max = 64)
        String code,

        @NotBlank
        @Size(max = 256)
        String title,

        @NotBlank
        @Size(max = 64)
        String teacherId,

        @Min(0)
        int plannedHours,

        @NotNull
        @Valid
        List<CourseEquipmentItemDto> equipmentRequirements,

        @NotNull
        List<UUID> groupIds
) {
}
