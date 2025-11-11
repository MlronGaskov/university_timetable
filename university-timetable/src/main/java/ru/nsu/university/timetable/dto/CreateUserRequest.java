package ru.nsu.university.timetable.dto;

import jakarta.validation.constraints.*;
import ru.nsu.university.timetable.domain.Role;

public record CreateUserRequest(
        @NotBlank @Size(max = 64) String login,
        @NotBlank @Email @Size(max = 128) String email,
        @NotNull Role role,
        @Size(min = 6, max = 100) String password // optional for admin to set; if null will be generated
) {}