package ru.nsu.university.timetable.schedule.course.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CourseEquipmentItemDto(
        @NotBlank
        @Size(max = 128)
        String name,

        @Min(1)
        int quantity
) {
}
