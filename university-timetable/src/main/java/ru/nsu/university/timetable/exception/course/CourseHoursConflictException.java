package ru.nsu.university.timetable.exception.course;

import java.util.UUID;

public class CourseHoursConflictException extends RuntimeException {
    public CourseHoursConflictException(UUID courseId, int newPlanned, int scheduled) {
        super("New planned hours (%d) is below already scheduled total (%d) for course %s"
                .formatted(newPlanned, scheduled, courseId));
    }
}
