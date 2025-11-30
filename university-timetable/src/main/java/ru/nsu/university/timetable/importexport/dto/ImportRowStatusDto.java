package ru.nsu.university.timetable.importexport.dto;

import lombok.Data;

@Data
public class ImportRowStatusDto {
    private int rowNumber;
    private boolean valid;
    private boolean hasWarnings;
    private String message; // для ошибки или предпреждения
}
