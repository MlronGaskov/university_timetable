package ru.nsu.university.timetable.dto.groups;

import jakarta.validation.constraints.Min;

public record UpdateSubgroupRequest(
        String name,
        @Min(0) Integer size
) {}
