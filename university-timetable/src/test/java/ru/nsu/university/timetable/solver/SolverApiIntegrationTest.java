package ru.nsu.university.timetable.solver;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.Duration;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;


@DisplayName("Incremental Prolog solver API integration tests")
class SolverApiIntegrationTest {

    private static final ObjectMapper MAPPER = new ObjectMapper();
    private static final HttpClient HTTP = HttpClient.newBuilder()
            .connectTimeout(Duration.ofSeconds(3))
            .build();

    private static String solverBaseUrl;

    private static final List<String> DAYS = List.of(
            "MONDAY", "TUESDAY", "WEDNESDAY", "THURSDAY", "FRIDAY", "SATURDAY"
    );

    private static final List<Map<String, Object>> DEFAULT_SLOTS = List.of(
            timeSlot("09:00", "10:35"),
            timeSlot("10:50", "12:25"),
            timeSlot("12:40", "14:15"),
            timeSlot("14:30", "16:05"),
            timeSlot("16:20", "17:55"),
            timeSlot("18:10", "19:45")
    );

    @BeforeAll
    static void waitForSolver() throws Exception {
        solverBaseUrl = Optional.ofNullable(System.getProperty("solver.base-url"))
                .or(() -> Optional.ofNullable(System.getenv("SOLVER_BASE_URL")))
                .orElse("http://localhost:5000");
        solverBaseUrl = stripTrailingSlash(solverBaseUrl);

        long deadline = System.nanoTime() + Duration.ofSeconds(40).toNanos();
        AssertionError lastError = null;

        while (System.nanoTime() < deadline) {
            try {
                HttpRequest request = HttpRequest.newBuilder()
                        .uri(URI.create(solverBaseUrl + "/health"))
                        .timeout(Duration.ofSeconds(2))
                        .GET()
                        .build();
                HttpResponse<String> response = HTTP.send(request, HttpResponse.BodyHandlers.ofString());
                if (response.statusCode() == 200 && response.body().contains("ok")) {
                    return;
                }
                lastError = new AssertionError("Unexpected /health response: " + response.statusCode() + " " + response.body());
            } catch (IOException | InterruptedException ex) {
                if (ex instanceof InterruptedException) {
                    Thread.currentThread().interrupt();
                }
                lastError = new AssertionError("Solver is not ready at " + solverBaseUrl, ex);
            }
            Thread.sleep(500);
        }

        if (lastError != null) {
            throw lastError;
        }
        fail("Solver did not become ready at " + solverBaseUrl);
    }

    @Test
    @DisplayName("/health returns ok")
    void healthEndpointWorks() throws Exception {
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(solverBaseUrl + "/health"))
                .timeout(Duration.ofSeconds(2))
                .GET()
                .build();

        HttpResponse<String> response = HTTP.send(request, HttpResponse.BodyHandlers.ofString());

        assertEquals(200, response.statusCode());
        JsonNode body = MAPPER.readTree(response.body());
        assertEquals("ok", body.path("status").asText());
    }

    @Test
    @DisplayName("Empty incremental request returns empty schedule")
    void emptyIncrementalRequestReturnsEmptySchedule() throws Exception {
        JsonNode response = solveOk(request(
                List.of(),
                List.of(),
                List.of(),
                List.of(),
                policy()
        ));

        assertEquals(0, array(response, "placedSlots").size());
        assertEquals(0, array(response, "fixedSlots").size());
        assertEquals(0, array(response, "scheduleSlots").size());
        assertEquals(0, array(response, "unplaced").size());
        assertEquals(1.0, response.path("evaluationScore").asDouble(), 1e-9);
        assertEquals(0.0, response.path("evaluationPenalty").asDouble(), 1e-9);
    }

    @Test
    @DisplayName("Places a single slot inside teacher working hours")
    void placesSingleSlotInsideTeacherWorkingHours() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "10:35"))),
                List.of(),
                List.of(toPlace("REQ-1", "MATH101", "T-1", List.of("G-1"), 20)),
                policy()
        );

        JsonNode response = solveOk(req);

        assertEquals(1, array(response, "placedSlots").size());
        assertEquals(0, array(response, "unplaced").size());

        JsonNode placed = firstPlaced(response);
        assertEquals("REQ-1", placed.path("requestId").asText());
        assertEquals("MATH101", placed.path("courseCode").asText());
        assertEquals("T-1", placed.path("teacherId").asText());
        assertEquals("A-101", placed.path("roomCode").asText());
        assertEquals("MONDAY", placed.path("dayOfWeek").asText());
        assertEquals("09:00", placed.path("startTime").asText());
        assertEquals("10:35", placed.path("endTime").asText());
        assertEquals("2026-02-01", placed.path("validFrom").asText());
        assertEquals("2026-06-01", placed.path("validUntil").asText());
        assertEquals("EVERY_WEEK", placed.path("weekPattern").asText());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Fixed slots are echoed and are not moved")
    void fixedSlotsAreEchoedAndNotMoved() throws Exception {
        Map<String, Object> fixed = fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35");
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(
                        teacher("T-1", interval("MONDAY", "09:00", "10:35")),
                        teacher("T-2", interval("TUESDAY", "10:50", "12:25"))
                ),
                List.of(fixed),
                List.of(toPlace("REQ-1", "PHYS101", "T-2", List.of("G-2"), 20)),
                policy()
        );

        JsonNode response = solveOk(req);

        assertFixedSlotUnchanged(response, fixed);
        assertEquals(1, array(response, "placedSlots").size());
        assertEquals(2, array(response, "scheduleSlots").size());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Generated slot does not use a room occupied by a fixed slot")
    void generatedSlotDoesNotUseRoomOccupiedByFixedSlot() throws Exception {
        Map<String, Object> fixed = fixed("FIX-ROOM", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35");
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(
                        teacher("T-1", interval("MONDAY", "09:00", "10:35")),
                        teacher("T-2", interval("MONDAY", "09:00", "10:35"))
                ),
                List.of(fixed),
                List.of(toPlace("REQ-1", "PHYS101", "T-2", List.of("G-2"), 20)),
                policy()
        );

        JsonNode response = solveOk(req);
        JsonNode placed = firstPlaced(response);

        assertEquals("MONDAY", placed.path("dayOfWeek").asText());
        assertEquals("09:00", placed.path("startTime").asText());
        assertEquals("10:35", placed.path("endTime").asText());
        assertEquals("B-201", placed.path("roomCode").asText());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Generated slot does not overlap a fixed slot of the same teacher")
    void generatedSlotDoesNotOverlapFixedTeacherSlot() throws Exception {
        Map<String, Object> fixed = fixed("FIX-TEACHER", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35");
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "12:25"))),
                List.of(fixed),
                List.of(toPlace("REQ-1", "PHYS101", "T-1", List.of("G-2"), 20)),
                policy()
        );

        JsonNode response = solveOk(req);
        JsonNode placed = firstPlaced(response);

        assertEquals("T-1", placed.path("teacherId").asText());
        assertEquals("MONDAY", placed.path("dayOfWeek").asText());
        assertEquals("10:50", placed.path("startTime").asText());
        assertEquals("12:25", placed.path("endTime").asText());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Generated slot does not overlap a fixed slot of the same group")
    void generatedSlotDoesNotOverlapFixedGroupSlot() throws Exception {
        Map<String, Object> fixed = fixed("FIX-GROUP", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35");
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(
                        teacher("T-1", interval("MONDAY", "09:00", "10:35")),
                        teacher("T-2", interval("MONDAY", "09:00", "12:25"))
                ),
                List.of(fixed),
                List.of(toPlace("REQ-1", "PHYS101", "T-2", List.of("G-1"), 20)),
                policy()
        );

        JsonNode response = solveOk(req);
        JsonNode placed = firstPlaced(response);

        assertEquals("T-2", placed.path("teacherId").asText());
        assertEquals(List.of("G-1"), groups(placed));
        assertEquals("MONDAY", placed.path("dayOfWeek").asText());
        assertEquals("10:50", placed.path("startTime").asText());
        assertEquals("12:25", placed.path("endTime").asText());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Room capacity requirement is respected")
    void roomCapacityRequirementIsRespected() throws Exception {
        Map<String, Object> req = request(
                List.of(room("SMALL", 20), room("BIG", 80)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "10:35"))),
                List.of(),
                List.of(toPlace("REQ-1", "BIG-LECTURE", "T-1", List.of("G-1"), 50)),
                policy()
        );

        JsonNode response = solveOk(req);

        assertEquals("BIG", firstPlaced(response).path("roomCode").asText());
        assertEquals(0, array(response, "unplaced").size());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Equipment requirements are respected")
    void equipmentRequirementsAreRespected() throws Exception {
        Map<String, Object> req = request(
                List.of(
                        room("NO-PROJECTOR", 40, item("whiteboard", 1)),
                        room("PROJECTOR", 40, item("projector", 1), item("whiteboard", 1))
                ),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "10:35"))),
                List.of(),
                List.of(toPlace("REQ-1", "PHYS101", "T-1", List.of("G-1"), 20, item("projector", 1))),
                policy()
        );

        JsonNode response = solveOk(req);

        assertEquals("PROJECTOR", firstPlaced(response).path("roomCode").asText());
        assertEquals(0, array(response, "unplaced").size());
        assertNoHardConflicts(scheduleSlots(response));
    }

    @Test
    @DisplayName("Several generated slots do not conflict with each other")
    void severalGeneratedSlotsDoNotConflictWithEachOther() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "16:05"))),
                List.of(),
                List.of(
                        toPlace("REQ-1", "MATH101", "T-1", List.of("G-1"), 20),
                        toPlace("REQ-2", "MATH101", "T-1", List.of("G-1"), 20),
                        toPlace("REQ-3", "MATH101", "T-1", List.of("G-1"), 20),
                        toPlace("REQ-4", "MATH101", "T-1", List.of("G-1"), 20)
                ),
                policyWithLimits(6, 6, 6, 6)
        );

        JsonNode response = solveOk(req);

        assertEquals(4, array(response, "placedSlots").size());
        assertEquals(0, array(response, "unplaced").size());
        assertNoHardConflicts(scheduleSlots(response));
        assertAllRequestIdsWerePlaced(response, Set.of("REQ-1", "REQ-2", "REQ-3", "REQ-4"));
    }

    @Test
    @DisplayName("Unplaced is returned when teacher has no allowed grid slots")
    void returnsUnplacedWhenTeacherHasNoAllowedGridSlots() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40)),
                List.of(teacher("T-1", interval("MONDAY", "20:00", "21:00"))),
                List.of(),
                List.of(toPlace("REQ-1", "MATH101", "T-1", List.of("G-1"), 20)),
                policy()
        );

        JsonNode response = solveOk(req);

        assertEquals(0, array(response, "placedSlots").size());
        assertEquals(1, array(response, "unplaced").size());
        assertEquals("REQ-1", array(response, "unplaced").get(0).path("requestId").asText());
        assertTrue(response.path("evaluationPenalty").asDouble() >= 1000.0);
    }

    @Test
    @DisplayName("Unplaced is returned when no room has required equipment")
    void returnsUnplacedWhenNoRoomHasRequiredEquipment() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40, item("whiteboard", 1))),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "10:35"))),
                List.of(),
                List.of(toPlace("REQ-1", "PHYS101", "T-1", List.of("G-1"), 20, item("projector", 1))),
                policy()
        );

        JsonNode response = solveOk(req);

        assertEquals(0, array(response, "placedSlots").size());
        assertEquals(1, array(response, "unplaced").size());
        assertEquals("REQ-1", array(response, "unplaced").get(0).path("requestId").asText());
        assertTrue(response.path("evaluationPenalty").asDouble() >= 1000.0);
    }

    @Test
    @DisplayName("Conflicting fixed room slots are rejected")
    void conflictingFixedRoomSlotsAreRejected() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40)),
                List.of(
                        teacher("T-1", interval("MONDAY", "09:00", "10:35")),
                        teacher("T-2", interval("MONDAY", "09:00", "10:35"))
                ),
                List.of(
                        fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35"),
                        fixed("FIX-2", "PHYS101", "T-2", List.of("G-2"), "A-101", "MONDAY", "09:00", "10:35")
                ),
                List.of(),
                policy()
        );

        JsonNode error = solveError(req, 400);

        assertEquals("FIXED_SLOTS_CONFLICT", error.path("error").asText());
        assertTrue(containsConflictType(error, "ROOM_CONFLICT"));
    }

    @Test
    @DisplayName("Conflicting fixed teacher slots are rejected")
    void conflictingFixedTeacherSlotsAreRejected() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "10:35"))),
                List.of(
                        fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35"),
                        fixed("FIX-2", "PHYS101", "T-1", List.of("G-2"), "B-201", "MONDAY", "09:00", "10:35")
                ),
                List.of(),
                policy()
        );

        JsonNode error = solveError(req, 400);

        assertEquals("FIXED_SLOTS_CONFLICT", error.path("error").asText());
        assertTrue(containsConflictType(error, "TEACHER_CONFLICT"));
    }

    @Test
    @DisplayName("Conflicting fixed group slots are rejected")
    void conflictingFixedGroupSlotsAreRejected() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(
                        teacher("T-1", interval("MONDAY", "09:00", "10:35")),
                        teacher("T-2", interval("MONDAY", "09:00", "10:35"))
                ),
                List.of(
                        fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35"),
                        fixed("FIX-2", "PHYS101", "T-2", List.of("G-1"), "B-201", "MONDAY", "09:00", "10:35")
                ),
                List.of(),
                policy()
        );

        JsonNode error = solveError(req, 400);

        assertEquals("FIXED_SLOTS_CONFLICT", error.path("error").asText());
        assertTrue(containsConflictType(error, "GROUP_CONFLICT"));
    }

    @Test
    @DisplayName("Fixed slot referencing unknown room is rejected")
    void fixedSlotReferencingUnknownRoomIsRejected() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "10:35"))),
                List.of(fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "UNKNOWN", "MONDAY", "09:00", "10:35")),
                List.of(),
                policy()
        );

        JsonNode error = solveError(req, 400);

        assertEquals("INVALID_REQUEST", error.path("error").asText());
        assertTrue(error.path("message").asText().contains("unknown room"));
    }

    @Test
    @DisplayName("Fixed slot outside teacher working hours does not fail but produces warning")
    void fixedSlotOutsideTeacherWorkingHoursProducesWarning() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40)),
                List.of(teacher("T-1", interval("TUESDAY", "09:00", "10:35"))),
                List.of(fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35")),
                List.of(),
                policy()
        );

        JsonNode response = solveOk(req);
        JsonNode warnings = response.path("warnings").path("fixedSlots");

        assertTrue(warnings.isArray());
        assertEquals(1, warnings.size());
        assertEquals("FIXED_SLOT_OUTSIDE_TEACHER_WORKING_HOURS", warnings.get(0).path("type").asText());
        assertEquals("FIX-1", warnings.get(0).path("slotId").asText());
        assertFixedSlotUnchanged(response, fixed("FIX-1", "MATH101", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35"));
    }

    @Test
    @DisplayName("Soft limit violations affect score but do not create hard conflicts")
    void softLimitViolationsAffectScoreButNoHardConflictsAppear() throws Exception {
        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40), room("C-301", 40)),
                List.of(teacher("T-1", interval("MONDAY", "09:00", "16:05"))),
                List.of(),
                List.of(
                        toPlace("REQ-1", "C-1", "T-1", List.of("G-1"), 20),
                        toPlace("REQ-2", "C-2", "T-1", List.of("G-2"), 20),
                        toPlace("REQ-3", "C-3", "T-1", List.of("G-3"), 20),
                        toPlace("REQ-4", "C-4", "T-1", List.of("G-4"), 20)
                ),
                policyWithLimits(2, 6, 2, 6)
        );

        JsonNode response = solveOk(req);

        assertEquals(4, array(response, "placedSlots").size());
        assertEquals(0, array(response, "unplaced").size());
        assertNoHardConflicts(scheduleSlots(response));
        assertTrue(response.path("evaluationPenalty").asDouble() > 0.0,
                "Expected a positive soft penalty because teacher has more than 2 pairs in a day");
        assertTrue(response.path("evaluationScore").asDouble() < 1.0);
    }

    @Test
    @DisplayName("Large mixed scenario with fixed and generated slots produces a valid schedule")
    void largeMixedScenarioProducesValidSchedule() throws Exception {
        List<Map<String, Object>> rooms = List.of(
                room("A-101", 35, item("projector", 1)),
                room("A-102", 45, item("whiteboard", 1)),
                room("B-201", 60, item("projector", 1), item("whiteboard", 1)),
                room("LAB-1", 30, item("computer", 20), item("projector", 1))
        );

        List<Map<String, Object>> teachers = List.of(
                teacher("T-1", interval("MONDAY", "09:00", "17:55"), interval("WEDNESDAY", "09:00", "17:55")),
                teacher("T-2", interval("MONDAY", "09:00", "17:55"), interval("TUESDAY", "09:00", "17:55")),
                teacher("T-3", interval("TUESDAY", "09:00", "17:55"), interval("THURSDAY", "09:00", "17:55")),
                teacher("T-4", interval("WEDNESDAY", "09:00", "17:55"), interval("FRIDAY", "09:00", "17:55")),
                teacher("T-5", interval("THURSDAY", "09:00", "17:55"), interval("SATURDAY", "09:00", "17:55"))
        );

        List<Map<String, Object>> fixed = List.of(
                fixed("FIX-1", "BASE-MATH", "T-1", List.of("G-1"), "A-101", "MONDAY", "09:00", "10:35"),
                fixed("FIX-2", "BASE-PHYS", "T-2", List.of("G-2"), "B-201", "MONDAY", "10:50", "12:25"),
                fixed("FIX-3", "BASE-CS", "T-3", List.of("G-3"), "LAB-1", "TUESDAY", "12:40", "14:15"),
                fixed("FIX-4", "BASE-HIST", "T-4", List.of("G-4"), "A-102", "WEDNESDAY", "14:30", "16:05")
        );

        List<Map<String, Object>> toPlace = List.of(
                toPlace("REQ-1", "MATH-ADV", "T-1", List.of("G-2"), 25),
                toPlace("REQ-2", "MATH-ADV", "T-1", List.of("G-2"), 25),
                toPlace("REQ-3", "PHYS-LAB", "T-2", List.of("G-1"), 25, item("projector", 1)),
                toPlace("REQ-4", "CS-LAB", "T-3", List.of("G-5"), 20, item("computer", 15)),
                toPlace("REQ-5", "CS-LAB", "T-3", List.of("G-5"), 20, item("computer", 15)),
                toPlace("REQ-6", "ENGLISH", "T-4", List.of("G-1", "G-3"), 30),
                toPlace("REQ-7", "ECON", "T-5", List.of("G-6"), 30),
                toPlace("REQ-8", "ECON", "T-5", List.of("G-6"), 30),
                toPlace("REQ-9", "DB", "T-2", List.of("G-4"), 40, item("projector", 1)),
                toPlace("REQ-10", "NETWORKS", "T-3", List.of("G-7"), 20, item("computer", 10)),
                toPlace("REQ-11", "DISCRETE", "T-1", List.of("G-8"), 35),
                toPlace("REQ-12", "SOFT-ENG", "T-4", List.of("G-2"), 35)
        );

        JsonNode response = solveOk(request(
                rooms,
                teachers,
                fixed,
                toPlace,
                policyWithSearchAndLimits(1, 1, 20, 6, 6, 6, 6)
        ));

        assertEquals(12, array(response, "placedSlots").size());
        assertEquals(0, array(response, "unplaced").size());
        assertEquals(16, array(response, "scheduleSlots").size());
        assertAllRequestIdsWerePlaced(response, Set.of(
                "REQ-1", "REQ-2", "REQ-3", "REQ-4", "REQ-5", "REQ-6",
                "REQ-7", "REQ-8", "REQ-9", "REQ-10", "REQ-11", "REQ-12"
        ));
        for (Map<String, Object> fixedSlot : fixed) {
            assertFixedSlotUnchanged(response, fixedSlot);
        }
        assertNoHardConflicts(scheduleSlots(response));
        assertAllGeneratedSlotsRespectTeacherWorkingHours(response, teachers);
    }

    @Test
    @DisplayName("Generated slots avoid a heavily occupied fixed prefix of the week")
    void generatedSlotsAvoidHeavilyOccupiedFixedPrefix() throws Exception {
        List<Map<String, Object>> fixed = new ArrayList<>();
        fixed.add(fixed("FIX-A", "C-A", "T-A", List.of("G-A"), "A-101", "MONDAY", "09:00", "10:35"));
        fixed.add(fixed("FIX-B", "C-B", "T-B", List.of("G-B"), "B-201", "MONDAY", "09:00", "10:35"));
        fixed.add(fixed("FIX-C", "C-C", "T-C", List.of("G-C"), "A-101", "MONDAY", "10:50", "12:25"));
        fixed.add(fixed("FIX-D", "C-D", "T-D", List.of("G-D"), "B-201", "MONDAY", "10:50", "12:25"));

        Map<String, Object> req = request(
                List.of(room("A-101", 40), room("B-201", 40)),
                List.of(
                        teacher("T-A", interval("MONDAY", "09:00", "12:25")),
                        teacher("T-B", interval("MONDAY", "09:00", "12:25")),
                        teacher("T-C", interval("MONDAY", "10:50", "12:25")),
                        teacher("T-D", interval("MONDAY", "10:50", "12:25")),
                        teacher("T-NEW", interval("MONDAY", "09:00", "16:05"))
                ),
                fixed,
                List.of(
                        toPlace("REQ-1", "NEW-1", "T-NEW", List.of("G-NEW-1"), 20),
                        toPlace("REQ-2", "NEW-2", "T-NEW", List.of("G-NEW-2"), 20)
                ),
                policyWithLimits(6, 6, 6, 6)
        );

        JsonNode response = solveOk(req);

        assertEquals(2, array(response, "placedSlots").size());
        assertEquals(0, array(response, "unplaced").size());
        for (JsonNode placed : array(response, "placedSlots")) {
            assertEquals("MONDAY", placed.path("dayOfWeek").asText());
            assertFalse(List.of("09:00", "10:50").contains(placed.path("startTime").asText()),
                    "Generated slot must avoid fully occupied first two Monday pairs: " + placed);
        }
        assertNoHardConflicts(scheduleSlots(response));
    }

    private static JsonNode solveOk(Map<String, Object> request) throws Exception {
        HttpResponse<String> response = postSolve(request);
        JsonNode body = MAPPER.readTree(response.body());

        assertEquals(200, response.statusCode(), () -> "Expected 200 but got " + response.statusCode() + ": " + response.body());
        assertFalse(body.hasNonNull("error"), () -> "Solver returned error: " + response.body());
        assertTrue(body.path("placedSlots").isArray(), "placedSlots must be an array");
        assertTrue(body.path("fixedSlots").isArray(), "fixedSlots must be an array");
        assertTrue(body.path("scheduleSlots").isArray(), "scheduleSlots must be an array");
        assertTrue(body.path("unplaced").isArray(), "unplaced must be an array");
        return body;
    }

    private static JsonNode solveError(Map<String, Object> request, int expectedStatus) throws Exception {
        HttpResponse<String> response = postSolve(request);
        JsonNode body = MAPPER.readTree(response.body());

        assertEquals(expectedStatus, response.statusCode(), () -> "Unexpected response: " + response.statusCode() + " " + response.body());
        assertTrue(body.hasNonNull("error"), () -> "Expected error body, got: " + response.body());
        return body;
    }

    private static HttpResponse<String> postSolve(Map<String, Object> request) throws IOException, InterruptedException {
        String json = MAPPER.writeValueAsString(request);
        HttpRequest httpRequest = HttpRequest.newBuilder()
                .uri(URI.create(solverBaseUrl + "/solve"))
                .timeout(Duration.ofSeconds(30))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(json))
                .build();
        return HTTP.send(httpRequest, HttpResponse.BodyHandlers.ofString());
    }

    private static Map<String, Object> request(
            List<Map<String, Object>> rooms,
            List<Map<String, Object>> teachers,
            List<Map<String, Object>> fixedSlots,
            List<Map<String, Object>> slotsToPlace,
            Map<String, Object> policy
    ) {
        return linkedMap(
                "semester", linkedMap(
                        "code", "SPRING-2026",
                        "startAt", "2026-02-01T00:00:00Z",
                        "endAt", "2026-06-01T00:00:00Z"
                ),
                "rooms", rooms,
                "teachers", teachers,
                "policy", policy,
                "fixedSlots", fixedSlots,
                "slotsToPlace", slotsToPlace
        );
    }

    private static Map<String, Object> policy() throws JsonProcessingException {
        return policyWithSearchAndLimits(1, 1, 20, 6, 6, 3, 4);
    }

    private static Map<String, Object> policyWithLimits(
            int maxTeacherPairsPerDay,
            int maxGroupPairsPerDay,
            int maxTeacherConsecutive,
            int maxGroupConsecutive
    ) throws JsonProcessingException {
        return policyWithSearchAndLimits(1, 1, 20,
                maxTeacherPairsPerDay, maxGroupPairsPerDay,
                maxTeacherConsecutive, maxGroupConsecutive);
    }

    private static Map<String, Object> policyWithSearchAndLimits(
            int restarts,
            int topK,
            int maxRoomsPerTask,
            int maxTeacherPairsPerDay,
            int maxGroupPairsPerDay,
            int maxTeacherConsecutive,
            int maxGroupConsecutive
    ) throws JsonProcessingException {
        Map<String, Object> grid = linkedMap(
                "version", 1,
                "days", DAYS,
                "slots", DEFAULT_SLOTS
        );
        Map<String, Object> breaks = linkedMap(
                "version", 1,
                "lunch", linkedMap(
                        "enabled", true,
                        "windowStart", "12:00",
                        "windowEnd", "15:00",
                        "minFreeMinutes", 30
                )
        );
        Map<String, Object> limits = linkedMap(
                "version", 1,
                "maxPairsPerDay", linkedMap(
                        "teacher", maxTeacherPairsPerDay,
                        "group", maxGroupPairsPerDay
                ),
                "maxConsecutivePairs", linkedMap(
                        "teacher", maxTeacherConsecutive,
                        "group", maxGroupConsecutive
                ),
                "timeBounds", linkedMap(
                        "earliestStart", "09:00",
                        "latestEnd", "19:45"
                )
        );
        Map<String, Object> weights = linkedMap(
                "version", 1,
                "hard", linkedMap("unplacedPair", 1000),
                "soft", linkedMap(
                        "missingLunch", 3,
                        "tooManyPairsPerDay", 2,
                        "tooManyConsecutive", 1
                )
        );
        Map<String, Object> search = linkedMap(
                "version", 1,
                "timeLimitMs", 1000,
                "restarts", restarts,
                "topK", topK,
                "maxRoomsPerTask", maxRoomsPerTask
        );

        return linkedMap(
                "gridJson", MAPPER.writeValueAsString(grid),
                "breaksJson", MAPPER.writeValueAsString(breaks),
                "limitsJson", MAPPER.writeValueAsString(limits),
                "weightsJson", MAPPER.writeValueAsString(weights),
                "searchJson", MAPPER.writeValueAsString(search)
        );
    }

    private static Map<String, Object> room(String code, int capacity, Map<String, Object>... items) {
        return linkedMap(
                "roomCode", code,
                "capacity", capacity,
                "items", List.of(items)
        );
    }

    private static Map<String, Object> teacher(String teacherId, Map<String, Object>... intervals) {
        return linkedMap(
                "teacherId", teacherId,
                "preferredWorkingHours", List.of(intervals)
        );
    }

    private static Map<String, Object> interval(String day, String startTime, String endTime) {
        return linkedMap(
                "day", day,
                "startTime", startTime,
                "endTime", endTime
        );
    }

    private static Map<String, Object> fixed(
            String slotId,
            String courseCode,
            String teacherId,
            List<String> groupIds,
            String roomCode,
            String dayOfWeek,
            String startTime,
            String endTime
    ) {
        return linkedMap(
                "slotId", slotId,
                "courseCode", courseCode,
                "teacherId", teacherId,
                "groupIds", groupIds,
                "roomCode", roomCode,
                "dayOfWeek", dayOfWeek,
                "startTime", startTime,
                "endTime", endTime
        );
    }

    private static Map<String, Object> toPlace(
            String requestId,
            String courseCode,
            String teacherId,
            List<String> groupIds,
            int requiredRoomCapacity,
            Map<String, Object>... equipmentRequirements
    ) {
        return linkedMap(
                "requestId", requestId,
                "courseCode", courseCode,
                "teacherId", teacherId,
                "groupIds", groupIds,
                "requiredRoomCapacity", requiredRoomCapacity,
                "equipmentRequirements", List.of(equipmentRequirements)
        );
    }

    private static Map<String, Object> item(String name, int quantity) {
        return linkedMap(
                "name", name,
                "quantity", quantity
        );
    }

    private static Map<String, Object> timeSlot(String start, String end) {
        return linkedMap("start", start, "end", end);
    }

    @SuppressWarnings("unchecked")
    private static Map<String, Object> linkedMap(Object... keyValues) {
        if (keyValues.length % 2 != 0) {
            throw new IllegalArgumentException("Expected even number of key/value arguments");
        }
        Map<String, Object> map = new LinkedHashMap<>();
        for (int i = 0; i < keyValues.length; i += 2) {
            map.put((String) keyValues[i], keyValues[i + 1]);
        }
        return map;
    }

    private static JsonNode firstPlaced(JsonNode response) {
        JsonNode placed = array(response, "placedSlots");
        assertTrue(placed.size() > 0, "Expected at least one placed slot");
        return placed.get(0);
    }

    private static JsonNode array(JsonNode node, String field) {
        JsonNode arr = node.path(field);
        assertTrue(arr.isArray(), field + " must be an array, body was: " + node);
        return arr;
    }

    private static List<JsonNode> scheduleSlots(JsonNode response) {
        List<JsonNode> slots = new ArrayList<>();
        for (JsonNode slot : array(response, "scheduleSlots")) {
            slots.add(slot);
        }
        return slots;
    }

    private static void assertFixedSlotUnchanged(JsonNode response, Map<String, Object> expectedFixed) {
        String expectedSlotId = (String) expectedFixed.get("slotId");
        JsonNode actual = null;
        for (JsonNode fixed : array(response, "fixedSlots")) {
            if (expectedSlotId.equals(fixed.path("slotId").asText())) {
                actual = fixed;
                break;
            }
        }
        assertNotNull(actual, "Fixed slot was not returned: " + expectedSlotId);

        assertEquals("FIXED", actual.path("source").asText());
        assertEquals(expectedFixed.get("courseCode"), actual.path("courseCode").asText());
        assertEquals(expectedFixed.get("teacherId"), actual.path("teacherId").asText());
        assertEquals(expectedFixed.get("roomCode"), actual.path("roomCode").asText());
        assertEquals(expectedFixed.get("dayOfWeek"), actual.path("dayOfWeek").asText());
        assertEquals(expectedFixed.get("startTime"), actual.path("startTime").asText());
        assertEquals(expectedFixed.get("endTime"), actual.path("endTime").asText());
        assertEquals(expectedFixed.get("groupIds"), groups(actual));
        assertEquals("2026-02-01", actual.path("validFrom").asText());
        assertEquals("2026-06-01", actual.path("validUntil").asText());
        assertEquals("EVERY_WEEK", actual.path("weekPattern").asText());
    }

    private static void assertAllRequestIdsWerePlaced(JsonNode response, Set<String> expectedRequestIds) {
        Set<String> actual = new HashSet<>();
        for (JsonNode slot : array(response, "placedSlots")) {
            actual.add(slot.path("requestId").asText());
        }
        assertEquals(expectedRequestIds, actual);
    }

    private static void assertNoHardConflicts(List<JsonNode> slots) {
        for (int i = 0; i < slots.size(); i++) {
            for (int j = i + 1; j < slots.size(); j++) {
                JsonNode a = slots.get(i);
                JsonNode b = slots.get(j);
                if (!sameDayAndOverlappingTime(a, b)) {
                    continue;
                }

                String message = "Conflict between " + slotDebug(a) + " and " + slotDebug(b);
                assertNotEquals(a.path("roomCode").asText(), b.path("roomCode").asText(), "Room conflict: " + message);
                assertNotEquals(a.path("teacherId").asText(), b.path("teacherId").asText(), "Teacher conflict: " + message);
                assertTrue(disjoint(groups(a), groups(b)), "Group conflict: " + message);
            }
        }
    }

    private static void assertAllGeneratedSlotsRespectTeacherWorkingHours(JsonNode response, List<Map<String, Object>> teachers) {
        Map<String, List<Map<String, Object>>> intervalsByTeacher = new LinkedHashMap<>();
        for (Map<String, Object> teacher : teachers) {
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> intervals = (List<Map<String, Object>>) teacher.get("preferredWorkingHours");
            intervalsByTeacher.put((String) teacher.get("teacherId"), intervals);
        }

        for (JsonNode slot : array(response, "placedSlots")) {
            String teacherId = slot.path("teacherId").asText();
            List<Map<String, Object>> intervals = intervalsByTeacher.get(teacherId);
            assertNotNull(intervals, "Unknown teacher in generated slot: " + slot);
            assertTrue(intervals.stream().anyMatch(interval -> intervalContains(interval, slot)),
                    "Generated slot outside teacher working hours: " + slot);
        }
    }

    private static boolean intervalContains(Map<String, Object> interval, JsonNode slot) {
        if (!interval.get("day").equals(slot.path("dayOfWeek").asText())) {
            return false;
        }
        int intervalStart = minutes((String) interval.get("startTime"));
        int intervalEnd = minutes((String) interval.get("endTime"));
        int slotStart = minutes(slot.path("startTime").asText());
        int slotEnd = minutes(slot.path("endTime").asText());
        return slotStart >= intervalStart && slotEnd <= intervalEnd;
    }

    private static boolean sameDayAndOverlappingTime(JsonNode a, JsonNode b) {
        if (!a.path("dayOfWeek").asText().equals(b.path("dayOfWeek").asText())) {
            return false;
        }
        int aStart = minutes(a.path("startTime").asText());
        int aEnd = minutes(a.path("endTime").asText());
        int bStart = minutes(b.path("startTime").asText());
        int bEnd = minutes(b.path("endTime").asText());
        return aStart < bEnd && bStart < aEnd;
    }

    private static int minutes(String time) {
        String[] parts = time.split(":");
        return Integer.parseInt(parts[0]) * 60 + Integer.parseInt(parts[1]);
    }

    private static List<String> groups(JsonNode slot) {
        JsonNode groupIds = slot.path("groupIds");
        assertTrue(groupIds.isArray(), "groupIds must be an array in slot: " + slot);
        List<String> result = new ArrayList<>();
        for (JsonNode groupId : groupIds) {
            result.add(groupId.asText());
        }
        return result;
    }

    private static boolean disjoint(List<String> left, List<String> right) {
        Set<String> seen = new HashSet<>(left);
        for (String value : right) {
            if (seen.contains(value)) {
                return false;
            }
        }
        return true;
    }

    private static boolean containsConflictType(JsonNode error, String type) {
        for (JsonNode conflict : error.path("conflicts")) {
            if (type.equals(conflict.path("type").asText())) {
                return true;
            }
        }
        return false;
    }

    private static String slotDebug(JsonNode slot) {
        String id = slot.hasNonNull("slotId") ? slot.path("slotId").asText() : slot.path("requestId").asText();
        return id + " "
                + slot.path("dayOfWeek").asText() + " "
                + slot.path("startTime").asText() + "-" + slot.path("endTime").asText()
                + " room=" + slot.path("roomCode").asText()
                + " teacher=" + slot.path("teacherId").asText()
                + " groups=" + groups(slot);
    }

    private static String stripTrailingSlash(String value) {
        while (value.endsWith("/")) {
            value = value.substring(0, value.length() - 1);
        }
        return value;
    }
}
