package ru.nsu.university.timetable.dto;

import lombok.Data;
import ru.nsu.university.timetable.schedule.ScheduleQualityMetrics;

import java.util.List;

@Data
public class GeneratedAlternativeDto {

    private List<GeneratedSessionDto> sessions;
    private ScheduleQualityMetrics quality;
}
