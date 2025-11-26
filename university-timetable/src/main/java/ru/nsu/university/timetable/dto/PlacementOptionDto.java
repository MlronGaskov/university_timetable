package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.time.LocalDate;

@Data
public class PlacementOptionDto {

    private LocalDate date;
    private Integer slotIndex;
    private boolean hasSoftConflicts;
    private String explanation;
}
