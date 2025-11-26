package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.List;

@Data
public class AssistedPlacementResponse {

    private List<PlacementOptionDto> options;
}
