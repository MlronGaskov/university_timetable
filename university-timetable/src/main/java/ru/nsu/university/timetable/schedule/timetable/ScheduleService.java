package ru.nsu.university.timetable.schedule.timetable;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.semester.SemesterRepository;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleResponse;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSlotDto;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSummaryResponse;
import ru.nsu.university.timetable.solver.PrologSolverClient;
import ru.nsu.university.timetable.solver.dto.SolverRequest;
import ru.nsu.university.timetable.solver.dto.SolverResponse;

import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class ScheduleService {

    private final ScheduleRepository scheduleRepository;
    private final SemesterRepository semesterRepository;
    private final PrologSolverClient solverClient;

    @Transactional(readOnly = true)
    public List<ScheduleSummaryResponse> getSchedulesForSemester(UUID semesterId) {
        return scheduleRepository.findBySemester_IdOrderByVersionDesc(semesterId).stream()
                .map(this::toSummaryDto)
                .toList();
    }

    @Transactional(readOnly = true)
    public ScheduleResponse getSchedule(UUID scheduleId) {
        Schedule schedule = scheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new IllegalArgumentException("Schedule not found: " + scheduleId));
        return toDetailsDto(schedule);
    }

    public ScheduleResponse generateSchedule(UUID semesterId) {
        Semester semester = semesterRepository.findById(semesterId)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + semesterId));

        int nextVersion = scheduleRepository.findTopBySemester_IdOrderByVersionDesc(semesterId)
                .map(s -> s.getVersion() + 1)
                .orElse(1);

        // TODO: собрать реальный SolverRequest из Course/Room/Teacher/Policy
        SolverRequest solverRequest = buildStubRequest(semester);

        SolverResponse solverResponse = solverClient.solve(solverRequest);

        Schedule schedule = Schedule.builder()
                .semester(semester)
                .version(nextVersion)
                .evaluationScore(solverResponse.evaluationScore())
                .build();

        solverResponse.slots().forEach(slot -> {
            ScheduleSlot entitySlot = ScheduleSlot.builder()
                    .schedule(schedule)
                    .courseId(slot.courseId())
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
    }

    private ScheduleSummaryResponse toSummaryDto(Schedule s) {
        return new ScheduleSummaryResponse(
                s.getId(),
                s.getSemester().getId(),
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
                        slot.getCourseId(),
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
                s.getSemester().getId(),
                s.getVersion(),
                s.getEvaluationScore(),
                s.getCreatedAt(),
                s.getUpdatedAt(),
                slots
        );
    }

    private SolverRequest buildStubRequest(Semester semester) {
        return new SolverRequest(
                new SolverRequest.SemesterDto(
                        semester.getId(),
                        semester.getStartAt(),
                        semester.getEndAt(),
                        semester.getPolicyId()
                ),
                List.of(),
                List.of(),
                List.of(),
                null
        );
    }
}
