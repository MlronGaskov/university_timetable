package ru.nsu.university.timetable.service;

import ru.nsu.university.timetable.dto.policies.PolicyImpactDto;
import ru.nsu.university.timetable.dto.policies.PolicySetDto;
import ru.nsu.university.timetable.dto.policies.PolicySetSummaryDto;
import ru.nsu.university.timetable.dto.policies.PolicySetUpdateRequest;

import java.util.List;
import java.util.UUID;

public interface PolicyService {

    PolicySetDto getActivePolicies();

    PolicySetDto getById(UUID id);

    List<PolicySetSummaryDto> getHistory();

    PolicySetDto createPolicySet(PolicySetUpdateRequest request, String currentUser);

    PolicySetDto activateDraft(UUID draftId, String currentUser);

    PolicySetDto restorePolicy(UUID policyId, String currentUser);

    PolicyImpactDto previewImpact(UUID policyId);
}
