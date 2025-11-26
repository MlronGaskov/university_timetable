package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
public class LocalRecomputeRequest {

    private UUID versionId;

    private UUID groupId;
    private UUID teacherId;
    private UUID roomId;

    private LocalDate fromDate;
    private LocalDate toDate;

    private boolean dryRun = true;
}
