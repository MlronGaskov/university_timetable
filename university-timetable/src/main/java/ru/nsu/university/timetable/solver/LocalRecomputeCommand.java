package ru.nsu.university.timetable.solver;

import lombok.Data;
import ru.nsu.university.timetable.schedule.ClassSession;
import ru.nsu.university.timetable.schedule.ScheduleVersion;
import ru.nsu.university.timetable.dto.LocalRecomputeRequest;

import java.util.List;

@Data
public class LocalRecomputeCommand {

    private ScheduleVersion version;
    private List<ClassSession> scopeSessions;
    private LocalRecomputeRequest request;

    public static LocalRecomputeCommand of(
            ScheduleVersion version,
            List<ClassSession> scope,
            LocalRecomputeRequest request
    ) {
        LocalRecomputeCommand cmd = new LocalRecomputeCommand();
        cmd.setVersion(version);
        cmd.setScopeSessions(scope);
        cmd.setRequest(request);
        return cmd;
    }
}
