package ru.nsu.university.timetable.user.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Size;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.auth.Role;

public record UpdateUserRequest(
        @Email
        @Size(max = 128)
        String email,

        Role role,

        Status status,

        @Size(max = 64)
        String teacherId,

        @Size(max = 64)
        String studentId
) {
}
