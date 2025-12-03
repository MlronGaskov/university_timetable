package ru.nsu.university.timetable.service.imports.parser;

import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;
import ru.nsu.university.timetable.exception.imports.ImportFormatException;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.*;

@Component
public class CsvTabularFileParser implements TabularFileParser {
    @Override
    public boolean supports(String filename, String contentType) {
        if (filename == null)
            return false;
        String lower = filename.toLowerCase();
        return lower.endsWith(".csv");
    }

    @Override
    public List<Map<String, String>> parse(MultipartFile file) {
        try (BufferedReader reader = new BufferedReader(
                new InputStreamReader(file.getInputStream(), StandardCharsets.UTF_8))) {

            String headerLine = reader.readLine();
            if (headerLine == null) {
                throw new ImportFormatException("Empty file");
            }

            String[] headers = headerLine.split(";");
            List<String> headerList = new ArrayList<>();
            for (String h : headers) {
                headerList.add(h.trim());
            }

            List<Map<String, String>> result = new ArrayList<>();

            String line = reader.readLine();
            if (line != null) {
                do {
                    if (line.isBlank()) {
                        continue;
                    }

                    String[] values = line.split(";");
                    Map<String, String> row = new HashMap<>();
                    for (int i = 0; i < headerList.size(); i++) {
                        String value = (i < values.length) ? values[i].trim() : "";
                        row.put(headerList.get(i), value);
                    }
                    result.add(row);
                } while ((line = reader.readLine()) != null);
            }
            return result;
        } catch (ImportFormatException e) {
            throw e;
        } catch (Exception e) {
            throw new ImportFormatException("Error reading CSV. UTF-8 excpected, ':'", e);
        }
    }
}
