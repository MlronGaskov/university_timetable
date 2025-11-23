package ru.nsu.university.timetable.dto.groups;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;

public record CreateSubgroupRequest(
        @NotBlank String name,
        @Min(0) int size
) {}
