package ru.nsu.university.timetable.service;

public class PolicySetNotFoundException extends RuntimeException {
    public PolicySetNotFoundException(String message) {
        super(message);
    }
}
