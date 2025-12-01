package ru.nsu.university.timetable.exception.policy;

public class PolicySetNotFoundException extends RuntimeException {
    public PolicySetNotFoundException(String message) {
        super(message);
    }
}
