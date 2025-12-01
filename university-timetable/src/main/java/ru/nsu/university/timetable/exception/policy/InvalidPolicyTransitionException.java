package ru.nsu.university.timetable.exception.policy;

public class InvalidPolicyTransitionException extends RuntimeException {
    public InvalidPolicyTransitionException(String message) {
        super(message);
    }
}
