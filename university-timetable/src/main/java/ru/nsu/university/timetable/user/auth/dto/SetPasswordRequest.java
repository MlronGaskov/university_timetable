package ru.nsu.university.timetable.user.auth.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record SetPasswordRequest(
        @NotBlank
        @Size(min = 6, max = 100)
        String newPassword
) {
}
