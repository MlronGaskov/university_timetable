package ru.nsu.university.timetable.schedule.timetable.solver;

import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.schedule.timetable.Schedule;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;
import ru.nsu.university.timetable.schedule.timetable.WeekPattern;
import ru.nsu.university.timetable.schedule.timetable.dto.UnplacedCourseDto;
import ru.nsu.university.timetable.solver.dto.SolverResponse;

import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Collection;
import java.util.List;

@Component
public class ScheduleSolverResponseMapper {

    public List<ScheduleSlot> toEntitySlots(Schedule schedule, Collection<SolverResponse.SlotDto> solverSlots) {
        if (solverSlots == null || solverSlots.isEmpty()) {
            return List.of();
        }

        return solverSlots.stream()
                .map(slot -> toEntitySlot(schedule, slot))
                .toList();
    }

    public ScheduleSlot toEntitySlot(Schedule schedule, SolverResponse.SlotDto slot) {
        return ScheduleSlot.builder()
                .schedule(schedule)
                .courseCode(slot.courseId())
                .roomCode(slot.roomCode())
                .dayOfWeek(DayOfWeek.valueOf(slot.dayOfWeek()))
                .startTime(LocalTime.parse(slot.startTime()))
                .endTime(LocalTime.parse(slot.endTime()))
                .validFrom(LocalDate.parse(slot.validFrom()))
                .validUntil(LocalDate.parse(slot.validUntil()))
                .weekPattern(slot.weekPattern() == null ? WeekPattern.EVERY_WEEK : slot.weekPattern())
                .build();
    }

    public List<UnplacedCourseDto> toUnplacedCourseDtos(SolverResponse response) {
        return response.unplacedOrEmpty().stream()
                .map(u -> new UnplacedCourseDto(u.courseId(), u.reason()))
                .toList();
    }
}
