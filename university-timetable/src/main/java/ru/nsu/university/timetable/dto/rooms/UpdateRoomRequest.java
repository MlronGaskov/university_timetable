package ru.nsu.university.timetable.dto.rooms;

import jakarta.validation.constraints.Min;

import java.util.List;

public record UpdateRoomRequest(
        String building,
        String number,
        @Min(0) Integer capacity,
        List<RoomEquipmentItem> equipment
) {}