package ru.nsu.university.timetable.dto.courses;

import ru.nsu.university.timetable.domain.ActivityForm;
import ru.nsu.university.timetable.domain.CourseStatus;

import java.util.Set;
import java.util.UUID;

public record CourseListItemDto(
        UUID id,
        String code,
        String title,
        String department,
        Set<ActivityForm> activityForms,
        CourseStatus status
) {
}
