package ru.nsu.university.timetable.dto.policies;

import jakarta.validation.constraints.NotBlank;
import lombok.Data;

@Data
public class PolicySetUpdateRequest {

    @NotBlank
    private String gridJson;

    @NotBlank
    private String breaksJson;

    @NotBlank
    private String limitsJson;

    @NotBlank
    private String travelMatrixJson;

    @NotBlank
    private String weightsJson;

    private boolean draft;

    private boolean requireFeasible = false;
}
