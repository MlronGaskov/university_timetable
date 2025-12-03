package ru.nsu.university.timetable.user.teacher.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CreateTeacherRequest(
        @NotBlank
        @Size(max = 64)
        String teacherId,

        @NotBlank
        @Size(max = 128)
        String fullName
) {
}
