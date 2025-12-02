package ru.nsu.university.timetable.importexport.service.handler;

import ru.nsu.university.timetable.importexport.dto.ImportPreviewDto;
import ru.nsu.university.timetable.importexport.dto.ImportResultDto;

import java.util.List;
import java.util.Map;

public interface ImportHandler {
    String getType(); // "students", "groups", ...

    ImportPreviewDto preview(List<Map<String, String>> rows);

    ImportResultDto commit(List<Map<String, String>> rows);
}
