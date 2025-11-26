package ru.nsu.university.timetable.dto;

import lombok.Data;
import ru.nsu.university.timetable.schedule.ScheduleQualityMetrics;
import ru.nsu.university.timetable.schedule.ScheduleVersionStatus;

import java.time.Instant;
import java.util.UUID;

@Data
public class ScheduleVersionDto {

    private UUID id;
    private String name;
    private UUID semesterId;
    private ScheduleVersionStatus status;
    private boolean published;
    private ScheduleQualityMetrics qualityMetrics;
    private Instant createdAt;
    private Instant updatedAt;
}
