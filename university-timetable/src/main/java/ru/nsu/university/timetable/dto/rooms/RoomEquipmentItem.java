package ru.nsu.university.timetable.dto.rooms;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;

public record RoomEquipmentItem(
        @NotBlank String name,
        @Min(0)   int quantity
) {}