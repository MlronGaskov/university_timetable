package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.List;

@Data
public class GenerateScheduleResponse {

    private List<ScheduleVersionDto> createdVersions;
    private List<MusConflictDto> mus;
}
