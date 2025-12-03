package ru.nsu.university.timetable.user.auth.dto;

import jakarta.validation.constraints.*;
import ru.nsu.university.timetable.user.auth.Role;

import java.util.UUID;

public record CreateUserRequest(
        @NotBlank
        @Size(max = 64)
        String login,

        @NotBlank
        @Email
        @Size(max = 128)
        String email,

        @NotNull
        Role role,

        @Size(min = 6, max = 100)
        String password,

        UUID teacherId,
        UUID studentId
) {
}
