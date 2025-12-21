package ru.nsu.university.timetable.schedule.timetable;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.policy.Policy;
import ru.nsu.university.timetable.catalog.policy.PolicyRepository;
import ru.nsu.university.timetable.catalog.room.Room;
import ru.nsu.university.timetable.catalog.room.RoomRepository;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.semester.SemesterRepository;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleResponse;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSlotDto;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSummaryResponse;
import ru.nsu.university.timetable.solver.PrologSolverClient;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.solver.dto.SolverResponse;
import ru.nsu.university.timetable.user.teacher.Teacher;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;

import java.time.LocalDate;
import java.time.LocalTime;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Service
@RequiredArgsConstructor
@Transactional
@Slf4j
public class ScheduleService {

    private final ScheduleRepository scheduleRepository;
    private final SemesterRepository semesterRepository;
    private final CourseRepository courseRepository;
    private final RoomRepository roomRepository;
    private final TeacherRepository teacherRepository;
    private final PolicyRepository policyRepository;
    private final PrologSolverClient solverClient;

    private static final DateTimeFormatter TIME_FORMATTER = DateTimeFormatter.ofPattern("HH:mm");

    @Transactional(readOnly = true)
    public List<ScheduleSummaryResponse> getSchedulesForSemester(String semesterCode) {
        return scheduleRepository.findBySemester_CodeOrderByVersionDesc(semesterCode).stream()
                .map(this::toSummaryDto)
                .toList();
    }

    @Transactional(readOnly = true)
    public ScheduleResponse getSchedule(UUID scheduleId) {
        Schedule schedule = scheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new IllegalArgumentException("Schedule not found: " + scheduleId));
        return toDetailsDto(schedule);
    }

    public ScheduleResponse generateSchedule(String semesterCode) {
        try {
            Semester semester = semesterRepository.findByCodeIgnoreCase(semesterCode)
                    .orElseThrow(() -> {
                        log.error("Semester not found by code={}", semesterCode);
                        return new IllegalArgumentException("Semester not found: " + semesterCode);
                    });

            int nextVersion = scheduleRepository.findTopBySemester_CodeOrderByVersionDesc(semesterCode)
                    .map(s -> s.getVersion() + 1)
                    .orElse(1);

            SolverRequest solverRequest = buildSolverRequest(semester);
            log.info("Calling solver for semester={} with {} courses, {} rooms, {} teachers",
                    semesterCode,
                    solverRequest.courses().size(),
                    solverRequest.rooms().size(),
                    solverRequest.teachers().size());

            SolverResponse solverResponse = solverClient.solve(solverRequest);

            log.info("Solver returned {} slots with score={}",
                    solverResponse.slots().size(),
                    solverResponse.evaluationScore());

            Schedule schedule = Schedule.builder()
                    .semester(semester)
                    .version(nextVersion)
                    .evaluationScore(solverResponse.evaluationScore())
                    .build();

            solverResponse.slots().forEach(slot -> {
                ScheduleSlot entitySlot = ScheduleSlot.builder()
                        .schedule(schedule)
                        .courseCode(slot.courseId())
                        .roomCode(slot.roomCode())
                        .dayOfWeek(java.time.DayOfWeek.valueOf(slot.dayOfWeek()))
                        .startTime(LocalTime.parse(slot.startTime()))
                        .endTime(LocalTime.parse(slot.endTime()))
                        .validFrom(LocalDate.parse(slot.validFrom()))
                        .validUntil(LocalDate.parse(slot.validUntil()))
                        .weekPattern(slot.weekPattern())
                        .build();
                schedule.addSlot(entitySlot);
            });

            Schedule saved = scheduleRepository.save(schedule);
            return toDetailsDto(saved);
        } catch (Exception ex) {
            log.error("Error while generating schedule for semesterCode={}", semesterCode, ex);
            throw ex;
        }
    }

    private ScheduleSummaryResponse toSummaryDto(Schedule s) {
        return new ScheduleSummaryResponse(
                s.getId(),
                s.getSemester().getCode(),
                s.getVersion(),
                s.getEvaluationScore(),
                s.getCreatedAt(),
                s.getUpdatedAt()
        );
    }

    private ScheduleResponse toDetailsDto(Schedule s) {
        List<ScheduleSlotDto> slots = s.getSlots().stream()
                .map(slot -> new ScheduleSlotDto(
                        slot.getId(),
                        slot.getCourseCode(),
                        slot.getRoomCode(),
                        slot.getDayOfWeek(),
                        slot.getStartTime(),
                        slot.getEndTime(),
                        slot.getValidFrom(),
                        slot.getValidUntil(),
                        slot.getWeekPattern()
                ))
                .toList();

        return new ScheduleResponse(
                s.getId(),
                s.getSemester().getCode(),
                s.getVersion(),
                s.getEvaluationScore(),
                s.getCreatedAt(),
                s.getUpdatedAt(),
                slots
        );
    }

    private SolverRequest buildSolverRequest(Semester semester) {
        // Загружаем Policy
        Policy policy = policyRepository.findByNameIgnoreCase(semester.getPolicyName())
                .orElseThrow(() -> new IllegalArgumentException("Policy not found: " + semester.getPolicyName()));

        // Загружаем курсы по кодам из семестра
        List<SolverRequest.CourseDto> courseDtos = new ArrayList<>();
        Set<String> teacherIds = new HashSet<>();

        for (String courseCode : semester.getCourseCodes()) {
            Course course = courseRepository.findByCodeIgnoreCase(courseCode)
                    .orElseThrow(() -> new IllegalArgumentException("Course not found: " + courseCode));

            teacherIds.add(course.getTeacherId());

            List<SolverRequest.EquipmentRequirementDto> equipmentReqs = course.getEquipmentRequirements().stream()
                    .map(req -> new SolverRequest.EquipmentRequirementDto(req.getName(), req.getQuantity()))
                    .toList();

            courseDtos.add(new SolverRequest.CourseDto(
                    course.getCode(),
                    course.getTeacherId(),
                    course.getGroupCodes(),
                    course.getPlannedHours(),
                    course.getRequiredRoomCapacity(),
                    equipmentReqs
            ));
        }

        // Загружаем комнаты по кодам из семестра
        List<SolverRequest.RoomDto> roomDtos = new ArrayList<>();
        for (String roomCode : semester.getRoomCodes()) {
            Room room = roomRepository.findByRoomCodeIgnoreCase(roomCode)
                    .orElseThrow(() -> new IllegalArgumentException("Room not found: " + roomCode));

            List<SolverRequest.RoomItemDto> items = room.getItems().stream()
                    .map(item -> new SolverRequest.RoomItemDto(item.getName(), item.getQuantity()))
                    .toList();

            roomDtos.add(new SolverRequest.RoomDto(
                    room.getRoomCode(),
                    room.getCapacity(),
                    items
            ));
        }

        // Загружаем преподавателей, которые ведут курсы
        List<SolverRequest.TeacherDto> teacherDtos = new ArrayList<>();
        for (String teacherId : teacherIds) {
            Teacher teacher = teacherRepository.findByTeacherId(teacherId)
                    .orElseThrow(() -> new IllegalArgumentException("Teacher not found: " + teacherId));

            List<SolverRequest.WorkingIntervalDto> workingHours = teacher.getPreferredWorkingHours().stream()
                    .map(wh -> new SolverRequest.WorkingIntervalDto(
                            wh.getDay().name(),
                            wh.getStartTime().format(TIME_FORMATTER),
                            wh.getEndTime().format(TIME_FORMATTER)
                    ))
                    .toList();

            teacherDtos.add(new SolverRequest.TeacherDto(
                    teacher.getTeacherId(),
                    workingHours
            ));
        }

        // Формируем Policy DTO
        SolverRequest.PolicyDto policyDto = new SolverRequest.PolicyDto(
                policy.getId().toString(),
                policy.getGridJson(),
                policy.getBreaksJson(),
                policy.getLimitsJson(),
                policy.getTravelMatrixJson(),
                policy.getWeightsJson()
        );

        // Формируем Semester DTO
        SolverRequest.SemesterDto semesterDto = new SolverRequest.SemesterDto(
                semester.getId().toString(),
                semester.getStartAt(),
                semester.getEndAt(),
                policy.getId().toString()
        );

        return new SolverRequest(
                semesterDto,
                courseDtos,
                roomDtos,
                teacherDtos,
                policyDto
        );
    }
}
