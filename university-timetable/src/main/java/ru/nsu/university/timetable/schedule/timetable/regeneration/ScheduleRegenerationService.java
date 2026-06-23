package ru.nsu.university.timetable.schedule.timetable.regeneration;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.semester.SemesterRepository;
import ru.nsu.university.timetable.schedule.timetable.*;
import ru.nsu.university.timetable.schedule.timetable.solver.ScheduleSolverRequestBuilder;
import ru.nsu.university.timetable.schedule.timetable.solver.ScheduleSolverResponseMapper;
import ru.nsu.university.timetable.solver.PrologSolverClient;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.solver.dto.SolverResponse;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

@Service
@Slf4j
@RequiredArgsConstructor
public class ScheduleRegenerationService {
    private final SemesterRepository semesterRepository;
    private final ScheduleRepository scheduleRepository;
    private final ScheduleGenerationLockRepository lockRepository;
    private final ScheduleRegenerationPlanner planner;
    private final ScheduleSolverRequestBuilder requestBuilder;
    private final ScheduleSolverResponseMapper responseMapper;
    private final ScheduleChangeNotificationService notificationService;
    private final PrologSolverClient solverClient;

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAfterCourseChanged(String courseCode) {
        regenerateAffectedSchedules(ScheduleChangeEvent.courseChanged(courseCode));
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAfterTeacherWorkingHoursChanged(String teacherId) {
        regenerateAffectedSchedules(ScheduleChangeEvent.teacherWorkingHoursChanged(teacherId));
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAfterRoomChanged(String roomCode) {
        regenerateAffectedSchedules(ScheduleChangeEvent.roomChanged(roomCode));
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAfterSemesterChanged(String semesterCode) {
        regenerateAffectedSchedules(ScheduleChangeEvent.semesterChanged(semesterCode));
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAfterPolicyChanged(String policyName) {
        regenerateAffectedSchedules(ScheduleChangeEvent.policyChanged(policyName));
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAfterGroupChanged(String groupCode) {
        regenerateAffectedSchedules(ScheduleChangeEvent.groupChanged(groupCode));
    }

    @Transactional(propagation = Propagation.MANDATORY)
    public void regenerateAffectedSchedules(ScheduleChangeEvent event) {
        // Regeneration must commit atomically with the source catalog mutation.
        for (String semesterCode : affectedSemesterCodes(event)) {
            regenerateSemesterOnce(semesterCode, event);
        }
    }

    private void regenerateSemesterOnce(String semesterCode, ScheduleChangeEvent event) {
        Semester semester = semesterRepository.findByCodeIgnoreCase(semesterCode)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + semesterCode));
        String canonicalSemesterCode = semester.getCode();

        acquireSemesterLock(canonicalSemesterCode);

        Schedule activeSchedule = scheduleRepository
                .findBySemester_CodeAndStatus(canonicalSemesterCode, ScheduleStatus.ACTIVE)
                .orElse(null);

        if (activeSchedule == null) {
            log.info("Skip schedule regeneration for semesterCode={} because active schedule does not exist", canonicalSemesterCode);
            return;
        }

        ScheduleRegenerationPlan plan = planner.buildPlan(semester, activeSchedule);
        if (!plan.hasChanges()) {
            log.info("Skip schedule regeneration for semesterCode={} because all slots are still valid", canonicalSemesterCode);
            return;
        }

        SolverRequest solverRequest = requestBuilder.buildForRegeneration(
                semester,
                plan.lockedSlots(),
                plan.coursesToRegenerate()
        );
        SolverResponse solverResponse = solverClient.solve(solverRequest);

        Schedule nextSchedule = Schedule.builder()
                .semester(semester)
                .version(activeSchedule.getVersion() + 1)
                .status(ScheduleStatus.ACTIVE)
                .baseSchedule(activeSchedule)
                .generationReason(event.reason())
                .evaluationScore(solverResponse.evaluationScore())
                .build();

        plan.lockedSlots().stream()
                .map(slot -> copySlot(nextSchedule, slot))
                .forEach(nextSchedule::addSlot);

        responseMapper.toEntitySlots(nextSchedule, solverResponse.placedSlotsOrEmpty())
                .forEach(nextSchedule::addSlot);

        activeSchedule.setStatus(ScheduleStatus.SUPERSEDED);
        scheduleRepository.saveAndFlush(activeSchedule);
        scheduleRepository.saveAndFlush(nextSchedule);
        notificationService.notifyUsersAboutChanges(semester, activeSchedule, nextSchedule, event);

        log.info(
                "Regenerated schedule for semesterCode={}: oldVersion={}, newVersion={}, regeneratedCourses={}",
                canonicalSemesterCode,
                activeSchedule.getVersion(),
                nextSchedule.getVersion(),
                plan.coursesToRegenerate().stream().map(c -> c.getCode()).toList()
        );
    }

    private void acquireSemesterLock(String semesterCode) {
        if (!lockRepository.existsById(semesterCode)) {
            try {
                lockRepository.saveAndFlush(new ScheduleGenerationLock(semesterCode));
            } catch (DataIntegrityViolationException ignored) {
                log.debug("Schedule generation lock already exists for semesterCode={}", semesterCode);
            }
        }

        lockRepository.findForUpdate(semesterCode)
                .orElseThrow(() -> new IllegalStateException("Could not acquire schedule generation lock: " + semesterCode));
    }

    private ScheduleSlot copySlot(Schedule targetSchedule, ScheduleSlot source) {
        return ScheduleSlot.builder()
                .schedule(targetSchedule)
                .courseCode(source.getCourseCode())
                .roomCode(source.getRoomCode())
                .dayOfWeek(source.getDayOfWeek())
                .startTime(source.getStartTime())
                .endTime(source.getEndTime())
                .validFrom(source.getValidFrom())
                .validUntil(source.getValidUntil())
                .weekPattern(source.getWeekPattern())
                .build();
    }

    private List<String> affectedSemesterCodes(ScheduleChangeEvent event) {
        Set<String> result = new LinkedHashSet<>();

        if (event.semesterCode() != null) {
            semesterRepository.findByCodeIgnoreCase(event.semesterCode()).ifPresent(s -> result.add(s.getCode()));
        }
        if (event.courseCode() != null) {
            semesterRepository.findByCourseCodeIgnoreCase(event.courseCode()).forEach(s -> result.add(s.getCode()));
        }
        if (event.teacherId() != null) {
            semesterRepository.findByTeacherIdIgnoreCase(event.teacherId()).forEach(s -> result.add(s.getCode()));
        }
        if (event.roomCode() != null) {
            semesterRepository.findByRoomCodeIgnoreCase(event.roomCode()).forEach(s -> result.add(s.getCode()));
        }
        if (event.policyName() != null) {
            semesterRepository.findByPolicyNameIgnoreCase(event.policyName()).forEach(s -> result.add(s.getCode()));
        }
        if (event.groupCode() != null) {
            semesterRepository.findByGroupCodeIgnoreCase(event.groupCode()).forEach(s -> result.add(s.getCode()));
        }

        return new ArrayList<>(result);
    }
}
