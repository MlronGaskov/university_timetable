package ru.nsu.university.timetable.dto.users;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.Size;
import ru.nsu.university.timetable.domain.Role;
import ru.nsu.university.timetable.domain.Status;

public record UpdateUserRequest(
        @Email @Size(max = 128) String email,
        Role role,
        Status status
) {}