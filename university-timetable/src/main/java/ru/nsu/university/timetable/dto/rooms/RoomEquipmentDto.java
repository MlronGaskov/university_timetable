package ru.nsu.university.timetable.dto.rooms;

import java.util.UUID;

public record RoomEquipmentDto(
        UUID id,
        String name,
        int quantity
) {}
