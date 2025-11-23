package ru.nsu.university.timetable.dto.groups;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CreateGroupRequest(
        @NotBlank
        @Size(max = 128)
        String name,

        @NotBlank
        @Size(max = 64)
        String code,

        @Min(0)
        int size
) {}
