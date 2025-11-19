package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class GenerateScheduleRequest {

    private UUID semesterId;
    private String name;

    private boolean allowAlternatives = true;
    private Integer alternativesLimit = 3;
}
