package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
public class ScheduleSliceRequest {

    private SliceType type;
    private UUID entityId;
    private LocalDate fromDate;
    private LocalDate toDate;
}
