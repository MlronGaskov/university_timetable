package ru.nsu.university.timetable.catalog.policy;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.policy.dto.CreatePolicyRequest;
import ru.nsu.university.timetable.catalog.policy.dto.PolicyResponse;
import ru.nsu.university.timetable.catalog.policy.dto.UpdatePolicyRequest;

import java.io.IOException;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class PolicyService {
    private final PolicyRepository repository;
    private final ObjectMapper objectMapper;

    public PolicyResponse create(CreatePolicyRequest req) {
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

        return map(repository.save(policy));
    }

    public PolicyResponse update(UUID id, UpdatePolicyRequest req) {
        Policy policy = repository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + id));

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

        return map(policy);
    }

    public void delete(UUID id) {
        if (!repository.existsById(id)) {
            throw new IllegalArgumentException("Policy not found: " + id);
        }
        repository.deleteById(id);
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
