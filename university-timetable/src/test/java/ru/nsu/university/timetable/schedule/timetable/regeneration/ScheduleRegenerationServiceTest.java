package ru.nsu.university.timetable.schedule.timetable.regeneration;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.semester.SemesterRepository;
import ru.nsu.university.timetable.schedule.timetable.ScheduleGenerationLock;
import ru.nsu.university.timetable.schedule.timetable.ScheduleGenerationLockRepository;
import ru.nsu.university.timetable.schedule.timetable.ScheduleRepository;
import ru.nsu.university.timetable.schedule.timetable.ScheduleStatus;
import ru.nsu.university.timetable.schedule.timetable.solver.ScheduleSolverRequestBuilder;
import ru.nsu.university.timetable.schedule.timetable.solver.ScheduleSolverResponseMapper;
import ru.nsu.university.timetable.solver.PrologSolverClient;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class ScheduleRegenerationServiceTest {
    @Mock
    private SemesterRepository semesterRepository;
    @Mock
    private ScheduleRepository scheduleRepository;
    @Mock
    private ScheduleGenerationLockRepository lockRepository;
    @Mock
    private ScheduleRegenerationPlanner planner;
    @Mock
    private ScheduleSolverRequestBuilder requestBuilder;
    @Mock
    private ScheduleSolverResponseMapper responseMapper;
    @Mock
    private ScheduleChangeNotificationService notificationService;
    @Mock
    private PrologSolverClient solverClient;

    @InjectMocks
    private ScheduleRegenerationService service;

    @Test
    void skipsRegenerationWhenNoActiveScheduleExists() {
        Semester semester = Semester.builder()
                .code("2026-S1")
                .startAt(Instant.parse("2026-09-01T00:00:00Z"))
                .endAt(Instant.parse("2026-12-31T00:00:00Z"))
                .build();

        when(semesterRepository.findByCourseCodeIgnoreCase("CS-101")).thenReturn(List.of(semester));
        when(semesterRepository.findByCodeIgnoreCase("2026-S1")).thenReturn(Optional.of(semester));
        when(lockRepository.existsById("2026-S1")).thenReturn(true);
        when(lockRepository.findForUpdate("2026-S1")).thenReturn(Optional.of(new ScheduleGenerationLock("2026-S1")));
        when(scheduleRepository.findBySemester_CodeAndStatus("2026-S1", ScheduleStatus.ACTIVE)).thenReturn(Optional.empty());

        service.regenerateAfterCourseChanged("CS-101");

        verify(planner, never()).buildPlan(any(), any());
        verify(requestBuilder, never()).buildForRegeneration(any(), any(), any());
        verify(solverClient, never()).solve(any());
        verify(notificationService, never()).notifyUsersAboutChanges(any(), any(), any(), any());
        verify(scheduleRepository, never()).saveAndFlush(any());
    }
}
