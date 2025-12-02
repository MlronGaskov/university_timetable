package ru.nsu.university.timetable.exception.imports;

public class ImportFormatException extends RuntimeException {
    public ImportFormatException(String message) {
        super(message);
    }

    public ImportFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}
