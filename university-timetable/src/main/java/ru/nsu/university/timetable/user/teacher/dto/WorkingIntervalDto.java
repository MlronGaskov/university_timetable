package ru.nsu.university.timetable.user.teacher.dto;

import jakarta.validation.constraints.NotNull;
import lombok.Builder;

import java.time.DayOfWeek;
import java.time.LocalTime;

@Builder
public record WorkingIntervalDto(
        @NotNull DayOfWeek day,
        @NotNull LocalTime startTime,
        @NotNull LocalTime endTime
) {
}
