package ru.nsu.university.timetable.exception.course;

public class DuplicateCourseCodeException extends RuntimeException {
    public DuplicateCourseCodeException(String code) {
        super("Course with code '%s' already exists".formatted(code));
    }
}
