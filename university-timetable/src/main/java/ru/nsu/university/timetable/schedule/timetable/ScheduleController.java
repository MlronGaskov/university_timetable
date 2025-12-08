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

    @GetMapping("/semesters/{semesterCode}/schedules")
    public List<ScheduleSummaryResponse> getSchedulesForSemester(@PathVariable String semesterCode) {
        return scheduleService.getSchedulesForSemester(semesterCode);
    }

    @GetMapping("/schedules/{scheduleId}")
    public ScheduleResponse getSchedule(@PathVariable UUID scheduleId) {
        return scheduleService.getSchedule(scheduleId);
    }

    @PostMapping("/semesters/{semesterCode}/schedules/generate")
    @PreAuthorize("hasRole('ADMIN')")
    public ScheduleResponse generateSchedule(@PathVariable String semesterCode) {
        return scheduleService.generateSchedule(semesterCode);
    }
}
