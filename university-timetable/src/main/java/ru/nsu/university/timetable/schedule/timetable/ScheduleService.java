package ru.nsu.university.timetable.schedule.timetable;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.semester.SemesterRepository;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleResponse;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSlotDto;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSummaryResponse;
import ru.nsu.university.timetable.schedule.timetable.dto.UnplacedCourseDto;
import ru.nsu.university.timetable.schedule.timetable.solver.ScheduleSolverRequestBuilder;
import ru.nsu.university.timetable.schedule.timetable.solver.ScheduleSolverResponseMapper;
import ru.nsu.university.timetable.solver.PrologSolverClient;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.solver.dto.SolverResponse;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
@Slf4j
public class ScheduleService {

    private final ScheduleRepository scheduleRepository;
    private final SemesterRepository semesterRepository;
    private final PrologSolverClient solverClient;
    private final ScheduleSolverRequestBuilder solverRequestBuilder;
    private final ScheduleSolverResponseMapper solverResponseMapper;

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

            SolverRequest solverRequest = solverRequestBuilder.buildForFullGeneration(semester);
            SolverResponse solverResponse = solverClient.solve(solverRequest);

            Schedule schedule = Schedule.builder()
                    .semester(semester)
                    .version(nextVersion)
                    .evaluationScore(solverResponse.evaluationScore())
                    .build();

            solverResponseMapper.toEntitySlots(schedule, solverResponse.placedSlotsOrEmpty())
                    .forEach(schedule::addSlot);

            Schedule saved = scheduleRepository.save(schedule);

            List<UnplacedCourseDto> unplaced = solverResponseMapper.toUnplacedCourseDtos(solverResponse);
            return toDetailsDto(saved, unplaced, solverResponse.evaluationPenalty());
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
        return toDetailsDto(s, List.of(), null);
    }

    private ScheduleResponse toDetailsDto(Schedule s, List<UnplacedCourseDto> unplaced, Double penalty) {
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
                penalty,
                s.getCreatedAt(),
                s.getUpdatedAt(),
                slots,
                unplaced
        );
    }
}
