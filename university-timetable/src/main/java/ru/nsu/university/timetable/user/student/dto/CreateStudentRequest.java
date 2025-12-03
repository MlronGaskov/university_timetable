package ru.nsu.university.timetable.user.student.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CreateStudentRequest(
        @NotBlank
        @Size(max = 128)
        String fullName,

        @NotBlank
        @Size(max = 64)
        String studentId
) {
}
