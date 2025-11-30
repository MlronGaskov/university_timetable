package ru.nsu.university.timetable.importexport.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import ru.nsu.university.timetable.importexport.dto.ImportPreviewDto;
import ru.nsu.university.timetable.importexport.dto.ImportResultDto;
import ru.nsu.university.timetable.importexport.parser.ImportFormatException;
import ru.nsu.university.timetable.importexport.parser.TabularFileParser;
import ru.nsu.university.timetable.importexport.service.handler.ImportHandler;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class ImportService {

    private final List<TabularFileParser> parsers;
    private final List<ImportHandler> handlers;

    private ImportHandler findHandler(String type) {
        return handlers.stream()
                .filter(h -> h.getType().equalsIgnoreCase(type))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unknown import type: " + type));
    }

    private TabularFileParser findParser(MultipartFile file) {
        String filename = file.getOriginalFilename();
        String contentType = file.getContentType();

        return parsers.stream()
                .filter(p -> p.supports(filename, contentType))
                .findFirst()
                .orElseThrow(() -> new ImportFormatException(
                        "Не найден подходящий парсер для файла " + filename
                ));
    }

    public ImportPreviewDto preview(String type, MultipartFile file) {
        ImportHandler handler = findHandler(type);
        TabularFileParser parser = findParser(file);

        List<Map<String, String>> rows = parser.parse(file);
        return handler.preview(rows);
    }

    public ImportResultDto commit(String type, MultipartFile file) {
        ImportHandler handler = findHandler(type);
        TabularFileParser parser = findParser(file);

        List<Map<String, String>> rows = parser.parse(file);
        return handler.commit(rows);
    }
}
