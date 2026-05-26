package ru.nsu.university.timetable.schedule.timetable.regeneration;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.timetable.Schedule;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;
import ru.nsu.university.timetable.schedule.timetable.WeekPattern;

import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ScheduleRegenerationPlannerTest {
    private ScheduleSlotValidityChecker validityChecker;
    private CourseRepository courseRepository;
    private ScheduleRegenerationPlanner planner;

    @BeforeEach
    void setUp() {
        validityChecker = mock(ScheduleSlotValidityChecker.class);
        courseRepository = mock(CourseRepository.class);
        planner = new ScheduleRegenerationPlanner(validityChecker, courseRepository);
    }

    @Test
    void buildPlan_keepsValidSlotsAndRegeneratesWholeInvalidCourse() {
        Semester semester = semester(List.of("DB", "OS"));
        ScheduleSlot dbSlot1 = slot("DB", DayOfWeek.MONDAY);
        ScheduleSlot dbSlot2 = slot("DB", DayOfWeek.WEDNESDAY);
        ScheduleSlot osSlot = slot("OS", DayOfWeek.FRIDAY);
        Schedule active = schedule(semester, List.of(dbSlot1, dbSlot2, osSlot));

        when(validityChecker.validate(semester, dbSlot1)).thenReturn(SlotValidationResult.invalid("teacher hours changed"));
        when(validityChecker.validate(semester, dbSlot2)).thenReturn(SlotValidationResult.ok());
        when(validityChecker.validate(semester, osSlot)).thenReturn(SlotValidationResult.ok());
        when(courseRepository.findByCodeIgnoreCase("db")).thenReturn(Optional.of(course("DB")));

        ScheduleRegenerationPlan plan = planner.buildPlan(semester, active);

        assertTrue(plan.hasChanges());
        assertEquals(List.of(osSlot), plan.lockedSlots());
        assertEquals(List.of("DB"), plan.coursesToRegenerate().stream().map(Course::getCode).toList());
        assertTrue(plan.invalidCourseCodes().contains("db"));
    }

    @Test
    void buildPlan_regeneratesCourseMissingFromActiveSchedule() {
        Semester semester = semester(List.of("DB", "OS"));
        ScheduleSlot dbSlot = slot("DB", DayOfWeek.MONDAY);
        Schedule active = schedule(semester, List.of(dbSlot));

        when(validityChecker.validate(semester, dbSlot)).thenReturn(SlotValidationResult.ok());
        when(courseRepository.findByCodeIgnoreCase("os")).thenReturn(Optional.of(course("OS")));

        ScheduleRegenerationPlan plan = planner.buildPlan(semester, active);

        assertTrue(plan.hasChanges());
        assertEquals(List.of(dbSlot), plan.lockedSlots());
        assertEquals(List.of("OS"), plan.coursesToRegenerate().stream().map(Course::getCode).toList());
        assertTrue(plan.missingCourseCodes().contains("os"));
    }

    @Test
    void buildPlan_hasNoChangesWhenAllSemesterCoursesAreScheduledAndValid() {
        Semester semester = semester(List.of("DB"));
        ScheduleSlot dbSlot = slot("DB", DayOfWeek.MONDAY);
        Schedule active = schedule(semester, List.of(dbSlot));

        when(validityChecker.validate(semester, dbSlot)).thenReturn(SlotValidationResult.ok());

        ScheduleRegenerationPlan plan = planner.buildPlan(semester, active);

        assertFalse(plan.hasChanges());
        assertEquals(List.of(dbSlot), plan.lockedSlots());
        assertTrue(plan.coursesToRegenerate().isEmpty());
        verifyNoInteractions(courseRepository);
    }

    private Semester semester(List<String> courseCodes) {
        return Semester.builder()
                .code("SPRING-2026")
                .startAt(Instant.parse("2026-02-01T00:00:00Z"))
                .endAt(Instant.parse("2026-06-01T00:00:00Z"))
                .policyName("default")
                .status(Status.ACTIVE)
                .courseCodes(new ArrayList<>(courseCodes))
                .roomCodes(new ArrayList<>(List.of("R1")))
                .build();
    }

    private Schedule schedule(Semester semester, List<ScheduleSlot> slots) {
        Schedule schedule = Schedule.builder()
                .semester(semester)
                .version(1)
                .slots(new ArrayList<>())
                .build();
        slots.forEach(schedule::addSlot);
        return schedule;
    }

    private ScheduleSlot slot(String courseCode, DayOfWeek day) {
        return ScheduleSlot.builder()
                .courseCode(courseCode)
                .roomCode("R1")
                .dayOfWeek(day)
                .startTime(LocalTime.parse("10:00"))
                .endTime(LocalTime.parse("11:30"))
                .validFrom(LocalDate.parse("2026-02-01"))
                .validUntil(LocalDate.parse("2026-06-01"))
                .weekPattern(WeekPattern.EVERY_WEEK)
                .build();
    }

    private Course course(String code) {
        return Course.builder()
                .code(code)
                .title(code)
                .teacherId("T1")
                .plannedHours(2)
                .requiredRoomCapacity(10)
                .groupCodes(new ArrayList<>())
                .equipmentRequirements(new ArrayList<>())
                .status(Status.ACTIVE)
                .build();
    }
}
