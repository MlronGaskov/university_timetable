package ru.nsu.university.timetable.exception.semester;

public class DuplicateSemesterCodeException extends RuntimeException {
    public DuplicateSemesterCodeException(String code) {
        super("Semester with code '" + code + "' already exists");
    }
}

