package ru.nsu.university.timetable.dto.teachers;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CreateTeacherRequest(
        @NotBlank @Size(max = 128) String fullName
) {}