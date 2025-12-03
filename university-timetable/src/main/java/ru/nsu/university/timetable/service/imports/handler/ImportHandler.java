package ru.nsu.university.timetable.service.imports.handler;

import ru.nsu.university.timetable.dto.imports.ImportPreviewDto;
import ru.nsu.university.timetable.dto.imports.ImportResultDto;

import java.util.List;
import java.util.Map;

public interface ImportHandler {
    String getType(); // "students", "groups", ...

    ImportPreviewDto preview(List<Map<String, String>> rows);

    ImportResultDto commit(List<Map<String, String>> rows);
}
