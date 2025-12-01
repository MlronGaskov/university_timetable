package ru.nsu.university.timetable.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.PolicySet;
import ru.nsu.university.timetable.domain.PolicySetStatus;
import ru.nsu.university.timetable.dto.policies.PolicyImpactDto;
import ru.nsu.university.timetable.dto.policies.PolicySetDto;
import ru.nsu.university.timetable.dto.policies.PolicySetSummaryDto;
import ru.nsu.university.timetable.dto.policies.PolicySetUpdateRequest;
import ru.nsu.university.timetable.repo.PolicySetRepository;

import java.io.IOException;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class PolicyServiceImpl implements PolicyService {

    private final PolicySetRepository repository;
    private final ObjectMapper objectMapper;

    @Override
    @Transactional(readOnly = true)
    public PolicySetDto getActivePolicies() {
        PolicySet entity = repository.findFirstByStatusOrderByCreatedAtDesc(PolicySetStatus.ACTIVE)
                .orElseThrow(() -> new PolicySetNotFoundException("Active policy set not found"));
        return PolicySetMapper.toDto(entity);
    }

    @Override
    @Transactional(readOnly = true)
    public PolicySetDto getById(UUID id) {
        PolicySet entity = repository.findById(id)
                .orElseThrow(() -> new PolicySetNotFoundException("Policy set not found: " + id));
        return PolicySetMapper.toDto(entity);
    }

    @Override
    @Transactional(readOnly = true)
    public List<PolicySetSummaryDto> getHistory() {
        return repository.findAllByOrderByCreatedAtDesc().stream()
                .map(PolicySetMapper::toSummaryDto)
                .collect(Collectors.toList());
    }

    @Override
    public PolicySetDto createPolicySet(PolicySetUpdateRequest request, String currentUser) {
        validateJson(request);

        boolean feasible = checkFeasibilityStub(request);
        if (!feasible && request.isRequireFeasible()) {
            throw new InvalidPolicyTransitionException("Policy changes make schedules infeasible");
        }

        long nextVersion = repository.findFirstByOrderByVersionDesc()
                .map(PolicySet::getVersion)
                .orElse(0L) + 1L;

        PolicySetStatus status = request.isDraft()
                ? PolicySetStatus.DRAFT
                : PolicySetStatus.ACTIVE;

        if (status == PolicySetStatus.ACTIVE) {
            repository.findFirstByStatusOrderByCreatedAtDesc(PolicySetStatus.ACTIVE)
                    .ifPresent(active -> active.setStatus(PolicySetStatus.ARCHIVED));
        }

        PolicySet entity = PolicySet.builder()
                .version(nextVersion)
                .status(status)
                .gridJson(request.getGridJson())
                .breaksJson(request.getBreaksJson())
                .limitsJson(request.getLimitsJson())
                .travelMatrixJson(request.getTravelMatrixJson())
                .weightsJson(request.getWeightsJson())
                .createdBy(currentUser)
                .build();

        PolicySet saved = repository.save(entity);

        if (status == PolicySetStatus.ACTIVE) {
            // TODO: интегрировать с системой расписаний/solver'ом.
            // scheduleService.flagSchedulesAffectedByPolicyChange(saved.getId());
        }

        return PolicySetMapper.toDto(saved);
    }

    @Override
    public PolicySetDto activateDraft(UUID draftId, String currentUser) {
        PolicySet draft = repository.findById(draftId)
                .orElseThrow(() -> new PolicySetNotFoundException("Draft policy set not found: " + draftId));

        if (draft.getStatus() != PolicySetStatus.DRAFT) {
            throw new InvalidPolicyTransitionException("Policy set is not a draft");
        }

        repository.findFirstByStatusOrderByCreatedAtDesc(PolicySetStatus.ACTIVE)
                .ifPresent(active -> active.setStatus(PolicySetStatus.ARCHIVED));

        draft.setStatus(PolicySetStatus.ACTIVE);
        draft.setCreatedBy(currentUser);

        // TODO: scheduleService.flagSchedulesAffectedByPolicyChange(draft.getId());

        return PolicySetMapper.toDto(draft);
    }

    @Override
    public PolicySetDto restorePolicy(UUID policyId, String currentUser) {
        PolicySet base = repository.findById(policyId)
                .orElseThrow(() -> new PolicySetNotFoundException("Policy set not found: " + policyId));

        long nextVersion = repository.findFirstByOrderByVersionDesc()
                .map(PolicySet::getVersion)
                .orElse(0L) + 1L;

        repository.findFirstByStatusOrderByCreatedAtDesc(PolicySetStatus.ACTIVE)
                .ifPresent(active -> active.setStatus(PolicySetStatus.ARCHIVED));

        PolicySet restored = PolicySet.builder()
                .version(nextVersion)
                .status(PolicySetStatus.ACTIVE)
                .gridJson(base.getGridJson())
                .breaksJson(base.getBreaksJson())
                .limitsJson(base.getLimitsJson())
                .travelMatrixJson(base.getTravelMatrixJson())
                .weightsJson(base.getWeightsJson())
                .createdBy(currentUser)
                .build();

        PolicySet saved = repository.save(restored);

        // TODO: scheduleService.flagSchedulesAffectedByPolicyChange(saved.getId());

        return PolicySetMapper.toDto(saved);
    }

    @Override
    @Transactional(readOnly = true)
    public PolicyImpactDto previewImpact(UUID policyId) {
        // Пока честная заглушка.
        // Здесь можно будет вызвать solver / аналитику и вернуть список затронутых расписаний.
        repository.findById(policyId)
                .orElseThrow(() -> new PolicySetNotFoundException("Policy set not found: " + policyId));

        return PolicyImpactDto.builder()
                .affectedScheduleIds(List.of())
                .affectedCount(0L)
                .build();
    }

    private void validateJson(PolicySetUpdateRequest request) {
        validateJsonField(request.getGridJson(), "gridJson");
        validateJsonField(request.getBreaksJson(), "breaksJson");
        validateJsonField(request.getLimitsJson(), "limitsJson");
        validateJsonField(request.getTravelMatrixJson(), "travelMatrixJson");
        validateJsonField(request.getWeightsJson(), "weightsJson");
    }

    private void validateJsonField(String json, String fieldName) {
        try {
            objectMapper.readTree(json);
        } catch (IOException e) {
            throw new InvalidPolicyFormatException("Invalid JSON in field " + fieldName, e);
        }
    }

    private boolean checkFeasibilityStub(PolicySetUpdateRequest request) {
        return true;
    }
}
