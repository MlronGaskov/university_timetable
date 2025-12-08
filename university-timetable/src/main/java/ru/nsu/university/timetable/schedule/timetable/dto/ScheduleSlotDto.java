package ru.nsu.university.timetable.schedule.timetable.dto;

import ru.nsu.university.timetable.schedule.timetable.WeekPattern;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.UUID;

public record ScheduleSlotDto(
        UUID id,
        UUID courseId,
        String roomCode,
        DayOfWeek dayOfWeek,
        LocalTime startTime,
        LocalTime endTime,
        LocalDate validFrom,
        LocalDate validUntil,
        WeekPattern weekPattern
) {
}
