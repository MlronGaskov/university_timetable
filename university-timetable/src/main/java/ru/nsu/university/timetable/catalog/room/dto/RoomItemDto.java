package ru.nsu.university.timetable.catalog.room.dto;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record RoomItemDto(
        @NotBlank
        @Size(max = 128)
        String name,

        @Min(0)
        int quantity
) {
}
