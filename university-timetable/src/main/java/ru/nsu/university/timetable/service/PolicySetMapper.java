package ru.nsu.university.timetable.service;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import ru.nsu.university.timetable.domain.PolicySet;
import ru.nsu.university.timetable.dto.policies.PolicySetDto;
import ru.nsu.university.timetable.dto.policies.PolicySetSummaryDto;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public final class PolicySetMapper {

    public static PolicySetDto toDto(PolicySet e) {
        if (e == null) {
            return null;
        }
        return PolicySetDto.builder()
                .id(e.getId())
                .version(e.getVersion())
                .status(e.getStatus())
                .gridJson(e.getGridJson())
                .breaksJson(e.getBreaksJson())
                .limitsJson(e.getLimitsJson())
                .travelMatrixJson(e.getTravelMatrixJson())
                .weightsJson(e.getWeightsJson())
                .createdAt(e.getCreatedAt())
                .updatedAt(e.getUpdatedAt())
                .createdBy(e.getCreatedBy())
                .build();
    }

    public static PolicySetSummaryDto toSummaryDto(PolicySet e) {
        if (e == null) {
            return null;
        }
        return PolicySetSummaryDto.builder()
                .id(e.getId())
                .version(e.getVersion())
                .status(e.getStatus())
                .createdAt(e.getCreatedAt())
                .createdBy(e.getCreatedBy())
                .build();
    }
}
