package ru.nsu.university.timetable.exception.course;

import java.util.UUID;

public class CourseNotFoundException extends RuntimeException {
    public CourseNotFoundException(UUID id) {
        super("Course with id '%s' not found".formatted(id));
    }

    public CourseNotFoundException(String code) {
        super("Course with code '%s' not found".formatted(code));
    }
}
