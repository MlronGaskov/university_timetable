package ru.nsu.university.timetable.dto.users;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record SetPasswordRequest(@NotBlank @Size(min = 6, max = 100) String newPassword) {}
