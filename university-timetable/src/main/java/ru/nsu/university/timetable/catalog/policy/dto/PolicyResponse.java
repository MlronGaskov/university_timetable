package ru.nsu.university.timetable.catalog.policy.dto;

import java.time.Instant;
import java.util.UUID;

public record PolicyResponse(
        UUID id,
        String name,
        String gridJson,
        String breaksJson,
        String limitsJson,
        String travelMatrixJson,
        String weightsJson,
        Instant createdAt,
        Instant updatedAt
) {
}
