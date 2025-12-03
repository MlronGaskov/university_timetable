package ru.nsu.university.timetable.catalog.policy.dto;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record CreatePolicyRequest(
        @NotBlank
        @Size(max = 128)
        String name,

        @NotBlank
        String gridJson,

        @NotBlank
        String breaksJson,

        @NotBlank
        String limitsJson,

        @NotBlank
        String travelMatrixJson,

        @NotBlank
        String weightsJson
) {
}
