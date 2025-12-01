package ru.nsu.university.timetable.dto.policies;

import lombok.Builder;
import lombok.Data;
import ru.nsu.university.timetable.domain.PolicySetStatus;

import java.time.Instant;
import java.util.UUID;

@Data
@Builder
public class PolicySetSummaryDto {
    private UUID id;
    private Long version;
    private PolicySetStatus status;
    private Instant createdAt;
    private String createdBy;
}
