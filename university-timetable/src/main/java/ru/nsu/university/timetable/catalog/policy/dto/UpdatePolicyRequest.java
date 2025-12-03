package ru.nsu.university.timetable.catalog.policy.dto;

import jakarta.validation.constraints.Size;

public record UpdatePolicyRequest(
        @Size(max = 128)
        String name,

        String gridJson,
        String breaksJson,
        String limitsJson,
        String travelMatrixJson,
        String weightsJson
) {
}
