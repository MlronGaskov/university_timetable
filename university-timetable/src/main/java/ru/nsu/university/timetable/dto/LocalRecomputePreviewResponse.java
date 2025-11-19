package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.util.List;

@Data
public class LocalRecomputePreviewResponse {

    private List<ClassSessionDto> sessionsToChange;
    private List<ClassSessionDto> proposedChanges;
    private List<MusConflictDto> mus;
}
