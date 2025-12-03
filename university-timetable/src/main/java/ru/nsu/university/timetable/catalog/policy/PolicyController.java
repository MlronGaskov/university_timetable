package ru.nsu.university.timetable.catalog.policy;

import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.catalog.policy.dto.CreatePolicyRequest;
import ru.nsu.university.timetable.catalog.policy.dto.PolicyResponse;
import ru.nsu.university.timetable.catalog.policy.dto.UpdatePolicyRequest;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/policies")
@RequiredArgsConstructor
public class PolicyController {
    private final PolicyService service;

    @GetMapping
    public List<PolicyResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public PolicyResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @GetMapping("/by-name/{name}")
    public PolicyResponse byName(@PathVariable String name) {
        return service.findByName(name);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public PolicyResponse create(@Valid @RequestBody CreatePolicyRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public PolicyResponse update(@PathVariable UUID id,
                                 @Valid @RequestBody UpdatePolicyRequest req) {
        return service.update(id, req);
    }

    @DeleteMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public void delete(@PathVariable UUID id) {
        service.delete(id);
    }
}
