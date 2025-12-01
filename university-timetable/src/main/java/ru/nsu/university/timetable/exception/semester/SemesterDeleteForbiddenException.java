package ru.nsu.university.timetable.exception.semester;

import java.util.UUID;

public class SemesterDeleteForbiddenException extends RuntimeException {
    public SemesterDeleteForbiddenException(UUID id) {
        super("Cannot delete semester " + id + " because it has associated courses or groups");
    }
}

