package ru.nsu.university.timetable.schedule.timetable.solver;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.catalog.policy.Policy;
import ru.nsu.university.timetable.catalog.policy.PolicyRepository;
import ru.nsu.university.timetable.catalog.room.Room;
import ru.nsu.university.timetable.catalog.room.RoomRepository;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.user.teacher.Teacher;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;
import ru.nsu.university.timetable.user.teacher.TeacherWorkingHours;

import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class ScheduleSolverRequestBuilder {

    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm");

    private final CourseRepository courseRepository;
    private final RoomRepository roomRepository;
    private final TeacherRepository teacherRepository;
    private final PolicyRepository policyRepository;
    private final WeeklySlotToPlaceGenerator weeklySlotToPlaceGenerator;

    public SolverRequest buildForFullGeneration(Semester semester) {
        Objects.requireNonNull(semester, "semester must not be null");

        Policy policy = policyRepository.findByNameIgnoreCase(semester.getPolicyName())
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + semester.getPolicyName()));

        List<Course> courses = loadSemesterCourses(semester);
        List<SolverRequest.SlotToPlaceDto> slotsToPlace = weeklySlotToPlaceGenerator.generate(courses);
        List<SolverRequest.RoomDto> rooms = loadSemesterRooms(semester);
        List<SolverRequest.TeacherDto> teachers = loadTeachersForCourses(courses);

        return new SolverRequest(
                toSemesterDto(semester),
                rooms,
                teachers,
                toPolicyDto(policy),
                List.of(),
                slotsToPlace
        );
    }

    public SolverRequest buildForRegeneration(
            Semester semester,
            List<ScheduleSlot> lockedSlots,
            List<Course> coursesToRegenerate
    ) {
        Objects.requireNonNull(semester, "semester must not be null");

        Policy policy = policyRepository.findByNameIgnoreCase(semester.getPolicyName())
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + semester.getPolicyName()));

        List<Course> allSemesterCourses = loadSemesterCourses(semester);
        Map<String, Course> coursesByCode = allSemesterCourses.stream()
                .collect(Collectors.toMap(
                        course -> normalize(course.getCode()),
                        Function.identity(),
                        (first, second) -> first
                ));

        List<SolverRequest.FixedSlotDto> fixedSlots = safeList(lockedSlots).stream()
                .map(slot -> toFixedSlotDto(slot, coursesByCode))
                .toList();

        List<SolverRequest.SlotToPlaceDto> slotsToPlace = weeklySlotToPlaceGenerator.generate(safeList(coursesToRegenerate));

        return new SolverRequest(
                toSemesterDto(semester),
                loadSemesterRooms(semester),
                loadTeachersForCourses(allSemesterCourses),
                toPolicyDto(policy),
                fixedSlots,
                slotsToPlace
        );
    }

    private SolverRequest.SemesterDto toSemesterDto(Semester semester) {
        return new SolverRequest.SemesterDto(
                semester.getCode(),
                semester.getStartAt(),
                semester.getEndAt()
        );
    }

    private SolverRequest.PolicyDto toPolicyDto(Policy policy) {
        return new SolverRequest.PolicyDto(
                policy.getId().toString(),
                policy.getGridJson(),
                policy.getBreaksJson(),
                policy.getLimitsJson(),
                policy.getTravelMatrixJson(),
                policy.getWeightsJson()
        );
    }

    private SolverRequest.FixedSlotDto toFixedSlotDto(ScheduleSlot slot, Map<String, Course> coursesByCode) {
        Course course = coursesByCode.get(normalize(slot.getCourseCode()));
        if (course == null) {
            throw new IllegalArgumentException("Course for fixed slot not found: " + slot.getCourseCode());
        }

        String slotId = slot.getId() == null
                ? slot.getCourseCode() + "@" + slot.getDayOfWeek() + "@" + slot.getStartTime()
                : slot.getId().toString();

        return new SolverRequest.FixedSlotDto(
                slotId,
                course.getCode(),
                course.getTeacherId(),
                safeList(course.getGroupCodes()),
                slot.getRoomCode(),
                slot.getDayOfWeek().name(),
                slot.getStartTime().format(TIME_FORMATTER),
                slot.getEndTime().format(TIME_FORMATTER)
        );
    }

    private List<Course> loadSemesterCourses(Semester semester) {
        List<Course> result = new ArrayList<>();
        for (String courseCode : safeList(semester.getCourseCodes())) {
            Course course = courseRepository.findByCodeIgnoreCase(courseCode)
                    .orElseThrow(() -> new IllegalArgumentException("Course not found: " + courseCode));
            result.add(course);
        }
        return result;
    }

    private List<SolverRequest.RoomDto> loadSemesterRooms(Semester semester) {
        List<SolverRequest.RoomDto> result = new ArrayList<>();
        for (String roomCode : safeList(semester.getRoomCodes())) {
            Room room = roomRepository.findByRoomCodeIgnoreCase(roomCode)
                    .orElseThrow(() -> new IllegalArgumentException("Room not found: " + roomCode));

            List<SolverRequest.RoomItemDto> items = safeList(room.getItems()).stream()
                    .map(item -> new SolverRequest.RoomItemDto(item.getName(), item.getQuantity()))
                    .toList();

            result.add(new SolverRequest.RoomDto(
                    room.getRoomCode(),
                    room.getCapacity(),
                    items
            ));
        }
        return result;
    }

    private List<SolverRequest.TeacherDto> loadTeachersForCourses(List<Course> courses) {
        Set<String> teacherIds = new LinkedHashSet<>();
        for (Course course : courses) {
            teacherIds.add(course.getTeacherId());
        }

        List<SolverRequest.TeacherDto> result = new ArrayList<>();
        for (String teacherId : teacherIds) {
            Teacher teacher = teacherRepository.findByTeacherId(teacherId)
                    .orElseThrow(() -> new IllegalArgumentException("Teacher not found: " + teacherId));

            List<SolverRequest.WorkingIntervalDto> workingHours = safeCollection(teacher.getPreferredWorkingHours()).stream()
                    .sorted(Comparator
                            .comparing(TeacherWorkingHours::getDay)
                            .thenComparing(TeacherWorkingHours::getStartTime)
                    )
                    .map(wh -> new SolverRequest.WorkingIntervalDto(
                            wh.getDay().name(),
                            wh.getStartTime().format(TIME_FORMATTER),
                            wh.getEndTime().format(TIME_FORMATTER)
                    ))
                    .toList();

            result.add(new SolverRequest.TeacherDto(
                    teacher.getTeacherId(),
                    workingHours
            ));
        }
        return result;
    }

    private String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    private static <T> Collection<T> safeCollection(Collection<T> collection) {
        return collection == null ? List.of() : collection;
    }

    private static <T> List<T> safeList(List<T> list) {
        return list == null ? List.of() : list;
    }
}
