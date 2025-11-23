package ru.nsu.university.timetable.dto.students;

import jakarta.validation.constraints.Size;

import java.util.UUID;

public record UpdateStudentRequest(
        @Size(max = 128) String fullName,
        @Size(max = 64)  String studentId,
        UUID groupId
) {}