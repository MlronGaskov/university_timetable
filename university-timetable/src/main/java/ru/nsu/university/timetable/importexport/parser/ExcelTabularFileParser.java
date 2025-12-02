package ru.nsu.university.timetable.importexport.parser;

import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * Заглушка для Excel-парсера.
 */
@Component
public class ExcelTabularFileParser implements TabularFileParser {

    @Override
    public boolean supports(String filename, String contentType) {
        if (filename == null) {
            return false;
        }
        String lower = filename.toLowerCase();
        return lower.endsWith(".xlsx") || lower.endsWith(".xls");
    }

    @Override
    public List<Map<String, String>> parse(MultipartFile file) {
        throw new ImportFormatException("Импорт из Excel пока не поддерживается");
    }
}
