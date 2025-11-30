package ru.nsu.university.timetable.importexport.dto;

import lombok.Data;

import java.util.List;

@Data
public class ImportResultDto {
    private int importedRows;
    private int skippedRows;
    private List<ImportRowStatusDto> rows; // финальный отчёт по ошибкам или предупреждениям
}
