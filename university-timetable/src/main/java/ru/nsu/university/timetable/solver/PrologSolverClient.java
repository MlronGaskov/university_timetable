package ru.nsu.university.timetable.solver;

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

    public PrologSolverClient(
            RestTemplate solverRestTemplate,
            @Value("${solver.base-url:http://university-timetable-solver:5000}") String baseUrl
    ) {
        this.restTemplate = solverRestTemplate;
        this.baseUrl = baseUrl.endsWith("/") ? baseUrl.substring(0, baseUrl.length() - 1) : baseUrl;
    }

    public SolverResponse solve(SolverRequest request) {
        String url = baseUrl + "/solve";

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

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
