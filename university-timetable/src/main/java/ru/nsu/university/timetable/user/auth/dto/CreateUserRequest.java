package ru.nsu.university.timetable.user.auth.dto;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import ru.nsu.university.timetable.user.auth.Role;

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

        @Size(max = 64)
        String teacherId,

        @Size(max = 64)
        String studentId
) {
}
