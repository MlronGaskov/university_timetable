package ru.nsu.university.timetable.service;

public class InvalidPolicyTransitionException extends RuntimeException {
    public InvalidPolicyTransitionException(String message) {
        super(message);
    }
}
