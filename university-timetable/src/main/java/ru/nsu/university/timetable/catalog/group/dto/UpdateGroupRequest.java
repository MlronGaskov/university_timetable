package ru.nsu.university.timetable.catalog.group.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Size;

public record UpdateGroupRequest(
        @Size(max = 128)
        String name,

        @Size(max = 64)
        String code,

        @Min(0)
        Integer size
) {
}
