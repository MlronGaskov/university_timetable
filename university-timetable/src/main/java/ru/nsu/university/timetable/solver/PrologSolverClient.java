package ru.nsu.university.timetable.solver;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.web.client.HttpStatusCodeException;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;
import ru.nsu.university.timetable.solver.dto.SolverErrorResponse;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.solver.dto.SolverResponse;

import java.util.Objects;

@Component
@Slf4j
public class PrologSolverClient {
    private final RestTemplate restTemplate;
    private final String baseUrl;
    private final ObjectMapper objectMapper;

    public PrologSolverClient(
            RestTemplate solverRestTemplate,
            @Value("${solver.base-url:http://university-timetable-solver-lb:5000}") String baseUrl,
            ObjectMapper objectMapper
    ) {
        this.restTemplate = solverRestTemplate;
        this.baseUrl = normalizeBaseUrl(baseUrl);
        this.objectMapper = objectMapper;
    }

    public SolverResponse solve(SolverRequest request) {
        log.info(request.toString());

        Objects.requireNonNull(request, "Solver request must not be null");

        String url = baseUrl + "/solve";
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);

        logRequest(request);

        HttpEntity<SolverRequest> entity = new HttpEntity<>(request, headers);

        try {
            ResponseEntity<SolverResponse> response = restTemplate.exchange(
                    url,
                    HttpMethod.POST,
                    entity,
                    SolverResponse.class
            );

            SolverResponse body = response.getBody();
            if (body == null) {
                throw new SolverException("Solver returned empty body", response.getStatusCode().value(), null);
            }

            return body;
        } catch (HttpStatusCodeException ex) {
            SolverErrorResponse error = readErrorResponse(ex);
            String message = buildErrorMessage(ex, error);
            log.error("Solver returned error. url={}, status={}, body={}",
                    url, ex.getStatusCode(), ex.getResponseBodyAsString(), ex);
            throw new SolverException(message, ex.getStatusCode().value(), error, ex);
        } catch (ResourceAccessException ex) {
            log.error("Could not reach solver at {}: {}", url, ex.getMessage(), ex);
            throw new SolverException("Could not reach solver: " + ex.getMessage(), null, null, ex);
        } catch (RestClientException ex) {
            log.error("Error calling solver at {}: {}", url, ex.getMessage(), ex);
            throw new SolverException("Failed to call solver: " + ex.getMessage(), null, null, ex);
        }
    }

    public boolean isHealthy() {
        String url = baseUrl + "/health";
        try {
            ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);
            return response.getStatusCode().is2xxSuccessful();
        } catch (RestClientException ex) {
            log.warn("Solver health check failed at {}: {}", url, ex.getMessage());
            return false;
        }
    }

    private void logRequest(SolverRequest request) {
        if (!log.isDebugEnabled()) {
            return;
        }

        try {
            String jsonRequest = objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(request);
            log.debug("Sending incremental request to solver:\n{}", jsonRequest);
        } catch (Exception ex) {
            log.warn("Could not serialize solver request for logging", ex);
        }
    }

    private SolverErrorResponse readErrorResponse(HttpStatusCodeException ex) {
        String body = ex.getResponseBodyAsString();
        if (body.isBlank()) {
            return null;
        }

        try {
            return objectMapper.readValue(body, SolverErrorResponse.class);
        } catch (Exception parseEx) {
            log.warn("Could not deserialize solver error body: {}", body, parseEx);
            return null;
        }
    }

    private String buildErrorMessage(HttpStatusCodeException ex, SolverErrorResponse error) {
        if (error == null) {
            return "Solver returned HTTP " + ex.getStatusCode().value();
        }

        if (error.message() != null && !error.message().isBlank()) {
            return error.error() + ": " + error.message();
        }

        return error.error() != null
                ? error.error()
                : "Solver returned HTTP " + ex.getStatusCode().value();
    }

    private static String normalizeBaseUrl(String rawBaseUrl) {
        String value = Objects.requireNonNullElse(rawBaseUrl, "http://university-timetable-solver-lb:5000").trim();
        if (value.endsWith("/")) {
            return value.substring(0, value.length() - 1);
        }
        return value;
    }
}
