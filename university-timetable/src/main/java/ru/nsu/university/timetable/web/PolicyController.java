package ru.nsu.university.timetable.web;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.policies.PolicyImpactDto;
import ru.nsu.university.timetable.dto.policies.PolicySetDto;
import ru.nsu.university.timetable.dto.policies.PolicySetSummaryDto;
import ru.nsu.university.timetable.dto.policies.PolicySetUpdateRequest;
import ru.nsu.university.timetable.service.PolicyService;

import jakarta.validation.Valid;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/policies")
@RequiredArgsConstructor
public class PolicyController {

    private final PolicyService policyService;
    private final ObjectMapper objectMapper;

    private String currentUser(Authentication auth) {
        return auth != null ? auth.getName() : "system";
    }

    @GetMapping("/current")
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER','TEACHER','STUDENT')")
    public PolicySetDto getCurrent() {
        return policyService.getActivePolicies();
    }

    @GetMapping("/current/export")
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER')")
    public ResponseEntity<String> exportCurrent(
            @RequestParam(name = "format", defaultValue = "json") String format
    ) throws JsonProcessingException {
        PolicySetDto current = policyService.getActivePolicies();

        String filename = "policies-v" + current.getVersion() + "." + format.toLowerCase();

        if ("yaml".equalsIgnoreCase(format)) {
            String yaml = toSimpleYaml(current);
            return ResponseEntity.ok()
                    .header(HttpHeaders.CONTENT_DISPOSITION,
                            "attachment; filename=\"" + filename + "\"")
                    .contentType(MediaType.valueOf("application/x-yaml"))
                    .body(yaml);
        } else {
            String json = objectMapper.writeValueAsString(current);
            return ResponseEntity.ok()
                    .header(HttpHeaders.CONTENT_DISPOSITION,
                            "attachment; filename=\"" + filename + "\"")
                    .contentType(new MediaType("application", "json", StandardCharsets.UTF_8))
                    .body(json);
        }
    }

    @PostMapping
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER')")
    public PolicySetDto createPolicySet(@Valid @RequestBody PolicySetUpdateRequest request,
                                        Authentication authentication) {
        return policyService.createPolicySet(request, currentUser(authentication));
    }

    @PostMapping("/{id}/activate")
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER')")
    public PolicySetDto activateDraft(@PathVariable("id") UUID id,
                                      Authentication authentication) {
        return policyService.activateDraft(id, currentUser(authentication));
    }

    @GetMapping("/history")
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER')")
    public List<PolicySetSummaryDto> history() {
        return policyService.getHistory();
    }

    @PostMapping("/{id}/restore")
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER')")
    public PolicySetDto restore(@PathVariable("id") UUID id,
                                Authentication authentication) {
        return policyService.restorePolicy(id, currentUser(authentication));
    }

    @GetMapping("/{id}/impact")
    @PreAuthorize("hasAnyRole('ADMIN','SCHEDULER')")
    public PolicyImpactDto previewImpact(@PathVariable("id") UUID id) {
        return policyService.previewImpact(id);
    }


    private String toSimpleYaml(PolicySetDto dto) throws JsonProcessingException {
        StringBuilder sb = new StringBuilder();
        sb.append("id: ").append(dto.getId()).append("\n");
        sb.append("version: ").append(dto.getVersion()).append("\n");
        sb.append("status: ").append(dto.getStatus()).append("\n");
        sb.append("createdAt: ").append(dto.getCreatedAt()).append("\n");
        sb.append("updatedAt: ").append(dto.getUpdatedAt()).append("\n");
        sb.append("createdBy: ").append(dto.getCreatedBy()).append("\n");

        sb.append("grid: |\n").append(indent(dto.getGridJson())).append("\n");
        sb.append("breaks: |\n").append(indent(dto.getBreaksJson())).append("\n");
        sb.append("limits: |\n").append(indent(dto.getLimitsJson())).append("\n");
        sb.append("travelMatrix: |\n").append(indent(dto.getTravelMatrixJson())).append("\n");
        sb.append("weights: |\n").append(indent(dto.getWeightsJson())).append("\n");

        return sb.toString();
    }

    private String indent(String s) {
        if (s == null) return "";
        String[] lines = s.split("\\R");
        StringBuilder sb = new StringBuilder();
        for (String line : lines) {
            sb.append("  ").append(line).append("\n");
        }
        return sb.toString();
    }
}
