package ru.nsu.university.timetable.catalog.room.dto;

import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record RoomResponse(
        UUID id,
        String building,
        String number,
        int capacity,
        Status status,
        Instant createdAt,
        Instant updatedAt,
        List<RoomItemDto> items
) {}
