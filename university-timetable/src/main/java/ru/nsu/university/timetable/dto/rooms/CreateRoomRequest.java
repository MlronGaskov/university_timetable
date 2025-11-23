package ru.nsu.university.timetable.dto.rooms;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;

import java.util.List;

public record CreateRoomRequest(
        @NotBlank String building,
        @NotBlank String number,
        @Min(0)   int capacity,
        List<RoomEquipmentItem> equipment
) {}
