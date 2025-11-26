package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.List;

@Data
public class MusConflictDto {

    private String description;
    private List<String> entities;
}
