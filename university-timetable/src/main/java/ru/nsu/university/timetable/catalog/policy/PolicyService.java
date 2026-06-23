package ru.nsu.university.timetable.catalog.policy;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.policy.dto.CreatePolicyRequest;
import ru.nsu.university.timetable.catalog.policy.dto.PolicyResponse;
import ru.nsu.university.timetable.catalog.policy.dto.UpdatePolicyRequest;
import ru.nsu.university.timetable.web.OptimisticLockingGuard;
import ru.nsu.university.timetable.schedule.timetable.regeneration.ScheduleRegenerationService;
import ru.nsu.university.timetable.web.RetriableTransactionExecutor;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PolicyService {
    private final PolicyRepository repository;
    private final ObjectMapper objectMapper;
    private final ScheduleRegenerationService scheduleRegenerationService;
    private final RetriableTransactionExecutor transactionExecutor;

    public PolicyResponse create(CreatePolicyRequest req) {
        return transactionExecutor.execute("policy.create", () -> {
            if (repository.existsByNameIgnoreCase(req.name())) {
                throw new IllegalArgumentException("Policy with name '%s' already exists".formatted(req.name()));
            }

            validateAllJson(
                    req.gridJson(),
                    req.breaksJson(),
                    req.limitsJson(),
                    req.travelMatrixJson(),
                    req.weightsJson()
            );

            Policy policy = Policy.builder()
                    .name(req.name())
                    .gridJson(req.gridJson())
                    .breaksJson(req.breaksJson())
                    .limitsJson(req.limitsJson())
                    .travelMatrixJson(req.travelMatrixJson())
                    .weightsJson(req.weightsJson())
                    .build();

            return map(repository.saveAndFlush(policy));
        });
    }

    public PolicyResponse update(UUID id, long expectedVersion, UpdatePolicyRequest req) {
        return transactionExecutor.execute("policy.update", () -> {
            Policy policy = findEntityForMutation(id, expectedVersion);

            if (req.name() != null && !req.name().equalsIgnoreCase(policy.getName())) {
                if (repository.existsByNameIgnoreCase(req.name())) {
                    throw new IllegalArgumentException("Policy with name '%s' already exists".formatted(req.name()));
                }
                policy.setName(req.name());
            }

            if (req.gridJson() != null) {
                validateJson(req.gridJson(), "gridJson");
                policy.setGridJson(req.gridJson());
            }

            if (req.breaksJson() != null) {
                validateJson(req.breaksJson(), "breaksJson");
                policy.setBreaksJson(req.breaksJson());
            }

            if (req.limitsJson() != null) {
                validateJson(req.limitsJson(), "limitsJson");
                policy.setLimitsJson(req.limitsJson());
            }

            if (req.travelMatrixJson() != null) {
                validateJson(req.travelMatrixJson(), "travelMatrixJson");
                policy.setTravelMatrixJson(req.travelMatrixJson());
            }

            if (req.weightsJson() != null) {
                validateJson(req.weightsJson(), "weightsJson");
                policy.setWeightsJson(req.weightsJson());
            }

            Policy saved = repository.saveAndFlush(policy);
            scheduleRegenerationService.regenerateAfterPolicyChanged(saved.getName());
            return map(saved);
        });
    }

    public void delete(UUID id, long expectedVersion) {
        transactionExecutor.execute("policy.delete", () -> {
            Policy policy = findEntityForMutation(id, expectedVersion);
            repository.delete(policy);
            repository.flush();
        });
    }

    @Transactional(readOnly = true)
    public List<PolicyResponse> findAll() {
        return repository.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public PolicyResponse findOne(UUID id) {
        return repository.findById(id)
                .map(this::map)
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + id));
    }

    @Transactional(readOnly = true)
    public PolicyResponse findByName(String name) {
        return repository.findByNameIgnoreCase(name)
                .map(this::map)
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + name));
    }

    private Policy findEntityForMutation(UUID id, long expectedVersion) {
        Policy policy = repository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + id));
        OptimisticLockingGuard.verifyVersion("Policy", id, expectedVersion, policy.getVersion());
        return policy;
    }

    private void validateAllJson(String grid, String breaks, String limits, String travel, String weights) {
        validateJson(grid, "gridJson");
        validateJson(breaks, "breaksJson");
        validateJson(limits, "limitsJson");
        validateJson(travel, "travelMatrixJson");
        validateJson(weights, "weightsJson");
    }

    private void validateJson(String json, String fieldName) {
        try {
            objectMapper.readTree(json);
        } catch (IOException e) {
            throw new IllegalArgumentException("Invalid JSON in field '%s'".formatted(fieldName), e);
        }
    }

    private PolicyResponse map(Policy p) {
        return new PolicyResponse(
                p.getId(),
                p.getVersion(),
                p.getName(),
                p.getGridJson(),
                p.getBreaksJson(),
                p.getLimitsJson(),
                p.getTravelMatrixJson(),
                p.getWeightsJson(),
                p.getCreatedAt(),
                p.getUpdatedAt()
        );
    }
}
