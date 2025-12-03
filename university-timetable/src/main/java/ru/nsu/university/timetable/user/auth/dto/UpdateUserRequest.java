package ru.nsu.university.timetable.user.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Size;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.auth.Role;

import java.util.UUID;

public record UpdateUserRequest(
        @Email
        @Size(max = 128)
        String email,

        Role role,

        Status status,

        UUID teacherId,
        UUID studentId
) {
}
