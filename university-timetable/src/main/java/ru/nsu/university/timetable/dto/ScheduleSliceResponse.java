package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.List;

@Data
public class ScheduleSliceResponse {

    private List<ClassSessionDto> sessions;
}
