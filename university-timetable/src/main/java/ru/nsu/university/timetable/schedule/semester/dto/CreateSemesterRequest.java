package ru.nsu.university.timetable.schedule.semester.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record CreateSemesterRequest(
        @NotBlank
        @Size(max = 64)
        String code,

        @NotNull
        Instant startAt,

        @NotNull
        Instant endAt,

        @NotNull
        String policyName,

        List<String> courseCodes,
        List<String> roomCodes
) {
}
