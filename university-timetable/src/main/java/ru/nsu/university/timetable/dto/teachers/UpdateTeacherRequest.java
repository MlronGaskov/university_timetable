package ru.nsu.university.timetable.dto.teachers;

import jakarta.validation.constraints.Size;

public record UpdateTeacherRequest(
        @Size(max = 128) String fullName
) {}