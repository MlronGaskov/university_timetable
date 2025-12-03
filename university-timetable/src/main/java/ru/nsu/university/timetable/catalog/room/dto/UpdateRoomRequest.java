package ru.nsu.university.timetable.catalog.room.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.Size;

import java.util.List;

public record UpdateRoomRequest(
        @Size(max = 64)
        String building,

        @Size(max = 64)
        String number,

        @Min(0)
        Integer capacity,

        List<RoomItemDto> items
) {}
