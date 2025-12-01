package ru.nsu.university.timetable.dto.policies;

import lombok.Builder;
import lombok.Data;
import ru.nsu.university.timetable.domain.PolicySetStatus;

import java.time.Instant;
import java.util.UUID;

@Data
@Builder
public class PolicySetDto {
    private UUID id;
    private Long version;
    private PolicySetStatus status;

    private String gridJson;
    private String breaksJson;
    private String limitsJson;
    private String travelMatrixJson;
    private String weightsJson;

    private Instant createdAt;
    private Instant updatedAt;
    private String createdBy;
}
