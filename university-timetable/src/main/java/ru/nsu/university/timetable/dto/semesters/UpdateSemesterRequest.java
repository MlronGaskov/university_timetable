package ru.nsu.university.timetable.dto.semesters;

import java.time.Instant;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

public record UpdateSemesterRequest(
        @NotBlank
        @Size(max = 64)
        String code,

        @NotNull
        Instant start,

        @NotNull
        Instant end
) {
}

