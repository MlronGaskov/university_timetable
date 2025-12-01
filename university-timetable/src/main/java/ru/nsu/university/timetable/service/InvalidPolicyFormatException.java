package ru.nsu.university.timetable.service;

public class InvalidPolicyFormatException extends RuntimeException {
    public InvalidPolicyFormatException(String message, Throwable cause) {
        super(message, cause);
    }
}
