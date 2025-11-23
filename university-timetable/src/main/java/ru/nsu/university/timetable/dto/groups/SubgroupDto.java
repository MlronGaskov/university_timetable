package ru.nsu.university.timetable.dto.groups;

import ru.nsu.university.timetable.domain.Status;

import java.util.UUID;

public record SubgroupDto(
        UUID id,
        String name,
        int size,
        Status status
) {}
