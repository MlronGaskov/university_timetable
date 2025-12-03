package ru.nsu.university.timetable.user.student.dto;

import jakarta.validation.constraints.Size;

public record UpdateStudentRequest(
        @Size(max = 128)
        String fullName,

        @Size(max = 64)
        String studentId
) {
}
