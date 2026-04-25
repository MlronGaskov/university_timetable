package ru.nsu.university.timetable.it;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class OptimisticLockingHttpIT {
    private static final String BASE_URL = stripTrailingSlash(
            System.getenv().getOrDefault("BASE_URL", "http://localhost:8080")
    );
    private static final String ADMIN_LOGIN =
            System.getenv().getOrDefault("ADMIN_LOGIN", "root");
    private static final String ADMIN_PASSWORD =
            System.getenv().getOrDefault("ADMIN_PASSWORD", "root");

    private static final ObjectMapper mapper = new ObjectMapper().findAndRegisterModules();
    private static final HttpClient client = HttpClient.newHttpClient();
    private static String adminToken;

    @BeforeAll
    static void login() throws Exception {
        HttpResponse<String> response = post(
                "/api/auth/login",
                null,
                Map.of("login", ADMIN_LOGIN, "password", ADMIN_PASSWORD)
        );

        assertEquals(200, response.statusCode(), response.body());
        adminToken = mapper.readTree(response.body()).get("token").asText();
    }

    @Test
    void groupUpdateRequiresFreshVersionAndSupportsRetry() throws Exception {
        JsonNode created = createGroup();

        String id = created.get("id").asText();
        long v0 = created.get("version").asLong();

        JsonNode first = expectJson(
                put("/api/groups/" + id, v0, Map.of("name", "First winner", "size", 21)),
                200
        );
        long v1 = first.get("version").asLong();

        expectJson(
                put("/api/groups/" + id, v0, Map.of("name", "Stale loser")),
                409
        );

        JsonNode retry = expectJson(
                put("/api/groups/" + id, v1, Map.of("name", "Retried winner")),
                200
        );

        assertEquals("Retried winner", retry.get("name").asText());
        assertTrue(retry.get("version").asLong() > v1);
    }

    @Test
    void groupArchiveRejectsStaleVersion() throws Exception {
        JsonNode created = createGroup();

        String id = created.get("id").asText();
        long v0 = created.get("version").asLong();

        JsonNode updated = expectJson(
                put("/api/groups/" + id, v0, Map.of("size", 33)),
                200
        );

        expectJson(
                post("/api/groups/" + id + "/archive", v0, null),
                409
        );

        JsonNode archived = expectJson(
                post("/api/groups/" + id + "/archive", updated.get("version").asLong(), null),
                200
        );

        assertEquals("INACTIVE", archived.get("status").asText());
    }

    @Test
    void policyUpdateRejectsStaleVersion() throws Exception {
        JsonNode created = expectJson(
                post("/api/policies", null, policyBody(unique("policy"))),
                200
        );

        String id = created.get("id").asText();
        long v0 = created.get("version").asLong();

        JsonNode first = expectJson(
                put("/api/policies/" + id, v0, Map.of("gridJson", "{\"updated\":true}")),
                200
        );

        expectJson(
                put("/api/policies/" + id, v0, Map.of("breaksJson", "{\"stale\":true}")),
                409
        );

        JsonNode retry = expectJson(
                put("/api/policies/" + id, first.get("version").asLong(), Map.of("breaksJson", "{\"retry\":true}")),
                200
        );

        assertEquals("{\"retry\":true}", retry.get("breaksJson").asText());
    }

    @Test
    void roomUpdateRejectsStaleVersion() throws Exception {
        String suffix = unique("room");

        JsonNode created = expectJson(
                post("/api/rooms", null, Map.of(
                        "roomCode", "R-" + suffix,
                        "building", "B-" + suffix,
                        "number", "101",
                        "capacity", 30,
                        "items", List.of(Map.of("name", "projector", "quantity", 1))
                )),
                200
        );

        String id = created.get("id").asText();
        long v0 = created.get("version").asLong();

        JsonNode first = expectJson(
                put("/api/rooms/" + id, v0, Map.of("capacity", 40)),
                200
        );

        expectJson(
                put("/api/rooms/" + id, v0, Map.of("capacity", 50)),
                409
        );

        JsonNode retry = expectJson(
                put("/api/rooms/" + id, first.get("version").asLong(), Map.of("capacity", 50)),
                200
        );

        assertEquals(50, retry.get("capacity").asInt());
    }

    @Test
    void studentUpdateRejectsStaleVersion() throws Exception {
        JsonNode created = expectJson(
                post("/api/students", null, Map.of(
                        "fullName", "Student " + unique("name"),
                        "studentId", unique("sid")
                )),
                200
        );

        String id = created.get("id").asText();
        long v0 = created.get("version").asLong();

        JsonNode first = expectJson(
                put("/api/students/" + id, v0, Map.of("fullName", "First Student Name")),
                200
        );

        expectJson(
                put("/api/students/" + id, v0, Map.of("fullName", "Stale Student Name")),
                409
        );

        JsonNode retry = expectJson(
                put("/api/students/" + id, first.get("version").asLong(), Map.of("fullName", "Retried Student Name")),
                200
        );

        assertEquals("Retried Student Name", retry.get("fullName").asText());
    }

    @Test
    void teacherUpdateRejectsStaleVersion() throws Exception {
        JsonNode created = expectJson(
                post("/api/teachers", null, Map.of(
                        "teacherId", unique("tid"),
                        "fullName", "Teacher " + unique("name")
                )),
                200
        );

        String id = created.get("id").asText();
        long v0 = created.get("version").asLong();

        String suffix = unique("teacher-update-name");
        String firstName = "First Teacher " + suffix;
        String staleName = "Stale Teacher " + suffix;
        String retryName = "Retried Teacher " + suffix;

        JsonNode first = expectJson(
                put("/api/teachers/" + id, v0, Map.of("fullName", firstName)),
                200
        );

        expectJson(
                put("/api/teachers/" + id, v0, Map.of("fullName", staleName)),
                409
        );

        JsonNode retry = expectJson(
                put("/api/teachers/" + id, first.get("version").asLong(), Map.of("fullName", retryName)),
                200
        );

        assertEquals(retryName, retry.get("fullName").asText());
    }

    @Test
    void missingIfMatchReturns428ForMutatingExistingResource() throws Exception {
        JsonNode created = createGroup();
        String id = created.get("id").asText();

        JsonNode error = expectJson(
                put("/api/groups/" + id, null, Map.of("name", "No version")),
                428
        );

        assertEquals(428, error.get("status").asInt());
    }

    private static JsonNode createGroup() throws Exception {
        String suffix = unique("group");
        return expectJson(
                post("/api/groups", null, Map.of(
                        "name", "Group " + suffix,
                        "code", "G-" + suffix,
                        "size", 10
                )),
                200
        );
    }

    private static Map<String, Object> policyBody(String name) {
        return Map.of(
                "name", name,
                "gridJson", "{}",
                "breaksJson", "{}",
                "limitsJson", "{}",
                "travelMatrixJson", "{}",
                "weightsJson", "{}"
        );
    }

    private static JsonNode expectJson(HttpResponse<String> response, int expectedStatus) throws IOException {
        assertEquals(expectedStatus, response.statusCode(), response.body());
        assertFalse(response.body() == null || response.body().isBlank(), "Empty response body");
        return mapper.readTree(response.body());
    }

    private static HttpResponse<String> get(String path) throws Exception {
        return send("GET", path, null, null);
    }

    private static HttpResponse<String> post(String path, Long ifMatch, Object body) throws Exception {
        return send("POST", path, ifMatch, body);
    }

    private static HttpResponse<String> put(String path, Long ifMatch, Object body) throws Exception {
        return send("PUT", path, ifMatch, body);
    }

    private static HttpResponse<String> send(String method, String path, Long ifMatch, Object body) throws Exception {
        HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create(BASE_URL + path))
                .header("Accept", "application/json");

        if (adminToken != null) {
            builder.header("Authorization", "Bearer " + adminToken);
        }
        if (ifMatch != null) {
            builder.header("If-Match", Long.toString(ifMatch));
        }

        if (body == null) {
            builder.method(method, HttpRequest.BodyPublishers.noBody());
        } else {
            builder.header("Content-Type", "application/json");
            builder.method(method, HttpRequest.BodyPublishers.ofString(mapper.writeValueAsString(body)));
        }

        return client.send(builder.build(), HttpResponse.BodyHandlers.ofString());
    }

    private static String unique(String prefix) {
        return prefix + "-" + Instant.now().toEpochMilli() + "-" + UUID.randomUUID().toString().substring(0, 8);
    }

    private static String stripTrailingSlash(String value) {
        if (value.endsWith("/")) {
            return value.substring(0, value.length() - 1);
        }
        return value;
    }
}
