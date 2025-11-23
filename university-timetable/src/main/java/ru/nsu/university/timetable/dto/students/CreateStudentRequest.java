package ru.nsu.university.timetable.dto.students;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

import java.util.UUID;

public record CreateStudentRequest(
        @NotBlank @Size(max = 128) String fullName,
        @NotBlank @Size(max = 64)  String studentId,
        UUID groupId
) {}