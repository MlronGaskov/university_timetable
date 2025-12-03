package ru.nsu.university.timetable.dto.imports;

import lombok.Data;

import java.util.List;

@Data
public class ImportPreviewDto {
    private int totalRows;
    private int validRows;
    private int invalidRows;

    private List<ImportRowStatusDto> rows;

    private List<String> detectedHeaders;
    private List<String> expectedHeaders;
}
