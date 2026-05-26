package ru.nsu.university.timetable.schedule.timetable.regeneration;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.room.Room;
import ru.nsu.university.timetable.catalog.room.RoomRepository;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;
import ru.nsu.university.timetable.schedule.timetable.WeekPattern;
import ru.nsu.university.timetable.user.teacher.Teacher;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;
import ru.nsu.university.timetable.user.teacher.TeacherWorkingHours;

import java.time.DayOfWeek;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class ScheduleSlotValidityCheckerTest {
    private CourseRepository courseRepository;
    private RoomRepository roomRepository;
    private TeacherRepository teacherRepository;
    private ScheduleSlotValidityChecker checker;

    @BeforeEach
    void setUp() {
        courseRepository = mock(CourseRepository.class);
        roomRepository = mock(RoomRepository.class);
        teacherRepository = mock(TeacherRepository.class);
        checker = new ScheduleSlotValidityChecker(courseRepository, roomRepository, teacherRepository);
    }

    @Test
    void validate_returnsValidWhenSlotStillFitsCurrentData() {
        Semester semester = semester();
        Course course = course("DB", "T1", 30, List.of(new Course.EquipmentRequirement("projector", 1)));
        Room room = room("R1", 40, List.of(new Room.RoomItem("projector", 1)));
        Teacher teacher = teacher("T1", workingHours(DayOfWeek.MONDAY, "09:00", "18:00"));

        when(courseRepository.findByCodeIgnoreCase("DB")).thenReturn(Optional.of(course));
        when(roomRepository.findByRoomCodeIgnoreCase("R1")).thenReturn(Optional.of(room));
        when(teacherRepository.findByTeacherId("T1")).thenReturn(Optional.of(teacher));

        SlotValidationResult result = checker.validate(semester, slot("DB", "R1", DayOfWeek.MONDAY, "10:00", "11:30"));

        assertTrue(result.valid());
    }

    @Test
    void validate_returnsInvalidWhenTeacherChangedWorkingHours() {
        Semester semester = semester();
        Course course = course("DB", "T1", 30, List.of());
        Room room = room("R1", 40, List.of());
        Teacher teacher = teacher("T1", workingHours(DayOfWeek.MONDAY, "12:00", "18:00"));

        when(courseRepository.findByCodeIgnoreCase("DB")).thenReturn(Optional.of(course));
        when(roomRepository.findByRoomCodeIgnoreCase("R1")).thenReturn(Optional.of(room));
        when(teacherRepository.findByTeacherId("T1")).thenReturn(Optional.of(teacher));

        SlotValidationResult result = checker.validate(semester, slot("DB", "R1", DayOfWeek.MONDAY, "10:00", "11:30"));

        assertFalse(result.valid());
        assertTrue(result.reason().contains("outside teacher working hours"));
    }

    @Test
    void validate_returnsInvalidWhenRoomHasNotEnoughEquipment() {
        Semester semester = semester();
        Course course = course("DB", "T1", 30, List.of(new Course.EquipmentRequirement("projector", 2)));
        Room room = room("R1", 40, List.of(new Room.RoomItem("projector", 1)));
        Teacher teacher = teacher("T1", workingHours(DayOfWeek.MONDAY, "09:00", "18:00"));

        when(courseRepository.findByCodeIgnoreCase("DB")).thenReturn(Optional.of(course));
        when(roomRepository.findByRoomCodeIgnoreCase("R1")).thenReturn(Optional.of(room));
        when(teacherRepository.findByTeacherId("T1")).thenReturn(Optional.of(teacher));

        SlotValidationResult result = checker.validate(semester, slot("DB", "R1", DayOfWeek.MONDAY, "10:00", "11:30"));

        assertFalse(result.valid());
        assertTrue(result.reason().contains("equipment"));
    }

    private Semester semester() {
        return Semester.builder()
                .code("SPRING-2026")
                .startAt(Instant.parse("2026-02-01T00:00:00Z"))
                .endAt(Instant.parse("2026-06-01T00:00:00Z"))
                .policyName("default")
                .status(Status.ACTIVE)
                .courseCodes(new ArrayList<>(List.of("DB")))
                .roomCodes(new ArrayList<>(List.of("R1")))
                .build();
    }

    private Course course(String code, String teacherId, int requiredCapacity, List<Course.EquipmentRequirement> requirements) {
        return Course.builder()
                .code(code)
                .title(code)
                .teacherId(teacherId)
                .plannedHours(2)
                .requiredRoomCapacity(requiredCapacity)
                .equipmentRequirements(new ArrayList<>(requirements))
                .groupCodes(new ArrayList<>())
                .status(Status.ACTIVE)
                .build();
    }

    private Room room(String code, int capacity, List<Room.RoomItem> items) {
        return Room.builder()
                .roomCode(code)
                .building("B")
                .number("101")
                .capacity(capacity)
                .items(new ArrayList<>(items))
                .status(Status.ACTIVE)
                .build();
    }

    private Teacher teacher(String teacherId, TeacherWorkingHours... workingHours) {
        return Teacher.builder()
                .teacherId(teacherId)
                .fullName("Teacher " + teacherId)
                .status(Status.ACTIVE)
                .preferredWorkingHours(new HashSet<>(List.of(workingHours)))
                .build();
    }

    private TeacherWorkingHours workingHours(DayOfWeek day, String start, String end) {
        return TeacherWorkingHours.builder()
                .day(day)
                .startTime(LocalTime.parse(start))
                .endTime(LocalTime.parse(end))
                .build();
    }

    private ScheduleSlot slot(String courseCode, String roomCode, DayOfWeek day, String start, String end) {
        return ScheduleSlot.builder()
                .courseCode(courseCode)
                .roomCode(roomCode)
                .dayOfWeek(day)
                .startTime(LocalTime.parse(start))
                .endTime(LocalTime.parse(end))
                .validFrom(LocalDate.parse("2026-02-01"))
                .validUntil(LocalDate.parse("2026-06-01"))
                .weekPattern(WeekPattern.EVERY_WEEK)
                .build();
    }
}
