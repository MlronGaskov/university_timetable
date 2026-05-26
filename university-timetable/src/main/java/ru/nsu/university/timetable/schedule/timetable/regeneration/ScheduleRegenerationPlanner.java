package ru.nsu.university.timetable.schedule.timetable.regeneration;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.timetable.Schedule;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;

import java.util.*;

@Component
@RequiredArgsConstructor
public class ScheduleRegenerationPlanner {
    private final ScheduleSlotValidityChecker validityChecker;
    private final CourseRepository courseRepository;

    public ScheduleRegenerationPlan buildPlan(Semester semester, Schedule activeSchedule) {
        Set<String> semesterCourseCodes = normalizeSet(semester.getCourseCodes());
        Set<String> scheduledCourseCodes = new LinkedHashSet<>();
        Set<String> invalidCourseCodes = new LinkedHashSet<>();

        for (ScheduleSlot slot : activeSchedule.getSlots()) {
            String normalizedCourseCode = normalize(slot.getCourseCode());
            scheduledCourseCodes.add(normalizedCourseCode);

            if (!semesterCourseCodes.contains(normalizedCourseCode)) {
                continue;
            }

            SlotValidationResult validation = validityChecker.validate(semester, slot);
            if (!validation.valid()) {
                invalidCourseCodes.add(normalizedCourseCode);
            }
        }

        Set<String> missingCourseCodes = new LinkedHashSet<>(semesterCourseCodes);
        missingCourseCodes.removeAll(scheduledCourseCodes);

        Set<String> coursesToRegenerateCodes = new LinkedHashSet<>();
        coursesToRegenerateCodes.addAll(invalidCourseCodes);
        coursesToRegenerateCodes.addAll(missingCourseCodes);

        List<ScheduleSlot> lockedSlots = activeSchedule.getSlots().stream()
                .filter(slot -> semesterCourseCodes.contains(normalize(slot.getCourseCode())))
                .filter(slot -> !coursesToRegenerateCodes.contains(normalize(slot.getCourseCode())))
                .toList();

        List<Course> coursesToRegenerate = coursesToRegenerateCodes.stream()
                .map(code -> courseRepository.findByCodeIgnoreCase(code)
                        .orElseThrow(() -> new IllegalArgumentException("Course not found: " + code)))
                .filter(course -> course.getStatus() == Status.ACTIVE)
                .toList();

        return new ScheduleRegenerationPlan(
                lockedSlots,
                coursesToRegenerate,
                invalidCourseCodes,
                missingCourseCodes
        );
    }

    private Set<String> normalizeSet(List<String> values) {
        Set<String> result = new LinkedHashSet<>();
        if (values == null) {
            return result;
        }
        for (String value : values) {
            if (value != null && !value.isBlank()) {
                result.add(normalize(value));
            }
        }
        return result;
    }

    private String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }
}
