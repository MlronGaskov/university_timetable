package ru.nsu.university.timetable.exception.semester;

public class InvalidSemesterDatesException extends RuntimeException {
    public InvalidSemesterDatesException() {
        super("Semester end date must be after start date");
    }
}

