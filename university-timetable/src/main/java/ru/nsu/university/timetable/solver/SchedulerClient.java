package ru.nsu.university.timetable.solver;

public interface SchedulerClient {

    SchedulerGenerationResult generateFull(GenerateScheduleCommand command);

    SchedulerLocalResult localRecompute(LocalRecomputeCommand command);

    // подсказки по размещению (для UC-SCHED-3)
    java.util.List<ru.nsu.university.timetable.dto.PlacementOptionDto> suggestPlacements(
            PlacementSuggestionCommand command
    );
}
