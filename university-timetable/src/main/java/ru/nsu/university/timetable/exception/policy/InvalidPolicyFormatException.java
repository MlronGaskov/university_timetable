package ru.nsu.university.timetable.exception.policy;

public class InvalidPolicyFormatException extends RuntimeException {
    public InvalidPolicyFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}
