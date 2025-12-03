package ru.nsu.university.timetable.user.teacher.dto;

import jakarta.validation.Valid;
import jakarta.validation.constraints.Size;

import java.util.List;

public record UpdateTeacherRequest(
        @Size(max = 128) String fullName,
        @Valid List<WorkingIntervalDto> preferredWorkingHours
) {}
