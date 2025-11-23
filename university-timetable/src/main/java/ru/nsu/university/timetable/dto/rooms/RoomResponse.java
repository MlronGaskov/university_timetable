package ru.nsu.university.timetable.dto.rooms;

import ru.nsu.university.timetable.domain.Status;

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
        List<RoomEquipmentDto> equipment
) {}
