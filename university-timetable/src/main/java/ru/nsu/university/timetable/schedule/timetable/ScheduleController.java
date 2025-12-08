package ru.nsu.university.timetable.schedule.timetable;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleResponse;
import ru.nsu.university.timetable.schedule.timetable.dto.ScheduleSummaryResponse;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class ScheduleController {

    private final ScheduleService scheduleService;

    @GetMapping("/semesters/{semesterId}/schedules")
    public List<ScheduleSummaryResponse> getSchedulesForSemester(@PathVariable UUID semesterId) {
        return scheduleService.getSchedulesForSemester(semesterId);
    }

    @GetMapping("/schedules/{scheduleId}")
    public ScheduleResponse getSchedule(@PathVariable UUID scheduleId) {
        return scheduleService.getSchedule(scheduleId);
    }

    @PostMapping("/semesters/{semesterId}/schedules/generate")
    @PreAuthorize("hasRole('ADMIN')")
    public ScheduleResponse generateSchedule(@PathVariable UUID semesterId) {
        return scheduleService.generateSchedule(semesterId);
    }
}
