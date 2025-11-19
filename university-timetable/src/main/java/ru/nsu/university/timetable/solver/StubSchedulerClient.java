package ru.nsu.university.timetable.solver;

import org.springframework.stereotype.Service;
import ru.nsu.university.timetable.dto.PlacementOptionDto;

import java.util.Collections;
import java.util.List;

@Service
public class StubSchedulerClient implements SchedulerClient {

    @Override
    public SchedulerGenerationResult generateFull(GenerateScheduleCommand command) {
        throw new UnsupportedOperationException("SchedulerClient.generateFull is not implemented yet");
    }

    @Override
    public SchedulerLocalResult localRecompute(LocalRecomputeCommand command) {
        throw new UnsupportedOperationException("SchedulerClient.localRecompute is not implemented yet");
    }

    @Override
    public List<PlacementOptionDto> suggestPlacements(PlacementSuggestionCommand command) {
        // Возвращаем пустой список, чтобы UI мог работать без солвера
        return Collections.emptyList();
    }
}
