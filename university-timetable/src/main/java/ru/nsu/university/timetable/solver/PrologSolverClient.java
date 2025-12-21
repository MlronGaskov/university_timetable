package ru.nsu.university.timetable.solver;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.*;
import org.springframework.stereotype.Component;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.solver.dto.SolverResponse;

@Component
@Slf4j
public class PrologSolverClient {
    private final RestTemplate restTemplate;
    private final String baseUrl;
    private final ObjectMapper objectMapper;

    public PrologSolverClient(
            RestTemplate solverRestTemplate,
            @Value("${solver.base-url:http://university-timetable-solver:5000}") String baseUrl,
            ObjectMapper objectMapper
    ) {
        this.restTemplate = solverRestTemplate;
        this.baseUrl = baseUrl.endsWith("/") ? baseUrl.substring(0, baseUrl.length() - 1) : baseUrl;
        this.objectMapper = objectMapper;
    }

    public SolverResponse solve(SolverRequest request) {
        String url = baseUrl + "/solve";

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        // Log the request for debugging
        try {
            String jsonRequest = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(request);
            log.info("Sending request to solver:\n{}", jsonRequest);
        } catch (Exception e) {
            log.warn("Could not serialize request for logging", e);
        }

        HttpEntity<SolverRequest> entity = new HttpEntity<>(request, headers);

        try {
            ResponseEntity<SolverResponse> response = restTemplate.exchange(
                    url,
                    HttpMethod.POST,
                    entity,
                    SolverResponse.class
            );

            if (!response.getStatusCode().is2xxSuccessful()) {
                throw new IllegalStateException("Solver returned non-2xx status: " + response.getStatusCode());
            }

            SolverResponse body = response.getBody();
            if (body == null) {
                throw new IllegalStateException("Solver returned empty body");
            }

            return body;
        } catch (RestClientException ex) {
            log.error("Error calling Prolog solver at {}: {}", url, ex.getMessage(), ex);
            throw new IllegalStateException("Failed to call Prolog solver", ex);
        }
    }
}
