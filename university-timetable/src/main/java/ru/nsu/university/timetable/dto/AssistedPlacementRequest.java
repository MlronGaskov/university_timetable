package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.UUID;

@Data
public class AssistedPlacementRequest {

    private UUID sessionId;
    private int topK = 5;
}
