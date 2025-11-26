package ru.nsu.university.timetable.solver;

import lombok.Data;
import ru.nsu.university.timetable.schedule.ClassSession;

@Data
public class PlacementSuggestionCommand {

    private ClassSession session;
    private int topK;

    public static PlacementSuggestionCommand of(ClassSession session, int topK) {
        PlacementSuggestionCommand cmd = new PlacementSuggestionCommand();
        cmd.setSession(session);
        cmd.setTopK(topK);
        return cmd;
    }
}
