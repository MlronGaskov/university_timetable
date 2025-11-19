package ru.nsu.university.timetable.solver;

import lombok.Data;
//TODO implement this use-case
// import ru.nsu.university.timetable.schedule.Semester;
import ru.nsu.university.timetable.dto.GenerateScheduleRequest;

@Data
public class GenerateScheduleCommand {

//TODO implement this use-case
//    private Semester semester;
    private GenerateScheduleRequest request;
//TODO implement this use-case
//    public static GenerateScheduleCommand of(Semester semester, GenerateScheduleRequest request) {
//        GenerateScheduleCommand cmd = new GenerateScheduleCommand();
//        cmd.setSemester(semester);
//        cmd.setRequest(request);
//        return cmd;
//    }
}
