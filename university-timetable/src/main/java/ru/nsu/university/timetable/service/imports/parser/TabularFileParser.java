package ru.nsu.university.timetable.service.imports.parser;

import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

public interface TabularFileParser {
    boolean supports(String filename, String contentType);

    List<Map<String, String>> parse(MultipartFile file);
}
