package ru.nsu.university.timetable.schedule.semester.dto;

import jakarta.validation.constraints.Size;

import java.time.Instant;
import java.util.List;
import java.util.UUID;

public record UpdateSemesterRequest(
        @Size(max = 64)
        String code,

        Instant startAt,
        Instant endAt,
        String policyName,
        List<String> courseCodes,
        List<String> roomCodes
) {
}
