package ru.nsu.university.timetable.user.teacher.dto;

import jakarta.validation.Valid;

import java.util.List;

public record UpdateWorkingHoursRequest(
        @Valid List<WorkingIntervalDto> preferredWorkingHours
) {
}
