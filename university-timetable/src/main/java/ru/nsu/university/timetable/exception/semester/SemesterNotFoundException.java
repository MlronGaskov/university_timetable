package ru.nsu.university.timetable.exception.semester;

import java.util.UUID;

public class SemesterNotFoundException extends RuntimeException {
    public SemesterNotFoundException(UUID id) {
        super("Semester not found: " + id);
    }
}

