package ru.nsu.university.timetable.solver;

import lombok.Data;
import ru.nsu.university.timetable.dto.GeneratedSessionDto;
import ru.nsu.university.timetable.dto.MusConflictDto;

import java.util.List;

@Data
public class SchedulerLocalResult {

    private boolean feasible;
    private List<GeneratedSessionDto> newSessions;
    private List<MusConflictDto> mus;
}
