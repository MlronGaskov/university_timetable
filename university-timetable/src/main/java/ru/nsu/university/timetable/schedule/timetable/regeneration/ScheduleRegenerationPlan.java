package ru.nsu.university.timetable.schedule.timetable.regeneration;

import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;

import java.util.List;
import java.util.Set;

public record ScheduleRegenerationPlan(
        List<ScheduleSlot> lockedSlots,
        List<Course> coursesToRegenerate,
        Set<String> invalidCourseCodes,
        Set<String> missingCourseCodes
) {
    public boolean hasChanges() {
        return !coursesToRegenerate.isEmpty();
    }
}
