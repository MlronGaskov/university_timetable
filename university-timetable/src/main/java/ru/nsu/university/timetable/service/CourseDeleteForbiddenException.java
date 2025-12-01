package ru.nsu.university.timetable.service;

public class CourseDeleteForbiddenException extends RuntimeException {
    public CourseDeleteForbiddenException(String message) {
        super(message);
    }
}
