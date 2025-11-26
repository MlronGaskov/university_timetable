package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
public class ClassSessionDto {

    private UUID id;
    private UUID versionId;
    private UUID courseId;
    private UUID teacherId;
    private UUID groupId;
    private UUID subgroupId;
    private UUID roomId;
    private LocalDate date;
    private Integer slotIndex;
    private Integer durationSlots;
    private boolean locked;
}
