package ru.nsu.university.timetable.dto.users;

import ru.nsu.university.timetable.domain.Role;
import ru.nsu.university.timetable.domain.Status;

import java.time.Instant;
import java.util.UUID;

public record UserResponse(
        UUID id,
        String login,
        String email,
        Status status,
        Role role,
        Instant createdAt,
        Instant updatedAt
) {}