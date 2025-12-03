package ru.nsu.university.timetable.catalog.room.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.util.List;

public record CreateRoomRequest(
        @NotBlank
        @Size(max = 64)
        String building,

        @NotBlank
        @Size(max = 64)
        String number,

        @Min(0)
        int capacity,

        List<RoomItemDto> items
) {}
