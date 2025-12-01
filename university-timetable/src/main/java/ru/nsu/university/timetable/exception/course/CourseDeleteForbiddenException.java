package ru.nsu.university.timetable.exception.course;

public class CourseDeleteForbiddenException extends RuntimeException {
    public CourseDeleteForbiddenException(String message) {
        super(message);
    }
}
