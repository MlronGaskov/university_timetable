package ru.nsu.university.timetable.solver;

import lombok.Data;
import ru.nsu.university.timetable.dto.GeneratedAlternativeDto;
import ru.nsu.university.timetable.dto.MusConflictDto;

import java.util.List;

@Data
public class SchedulerGenerationResult {

    private boolean feasible;
    private List<GeneratedAlternativeDto> alternatives;
    private List<MusConflictDto> mus;
}
