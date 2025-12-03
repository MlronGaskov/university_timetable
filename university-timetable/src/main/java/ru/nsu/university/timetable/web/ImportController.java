package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import ru.nsu.university.timetable.service.imports.ImportService;
import ru.nsu.university.timetable.dto.imports.ImportPreviewDto;
import ru.nsu.university.timetable.dto.imports.ImportResultDto;
import ru.nsu.university.timetable.exception.imports.ImportFormatException;

@RestController
@RequestMapping("/api/import")
@RequiredArgsConstructor
public class ImportController {

    private final ImportService importService;

    @PostMapping("/{type}/preview")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ImportPreviewDto> preview(
            @PathVariable String type,
            @RequestParam("file") MultipartFile file
    ) {
        ImportPreviewDto dto = importService.preview(type, file);
        return ResponseEntity.ok(dto);
    }

    @PostMapping("/{type}/commit")
    @PreAuthorize("hasRole('ADMIN')")
    public ResponseEntity<ImportResultDto> commit(
            @PathVariable String type,
            @RequestParam("file") MultipartFile file
    ) {
        ImportResultDto dto = importService.commit(type, file);
        return ResponseEntity.ok(dto);
    }

    @ExceptionHandler(ImportFormatException.class)
    public ResponseEntity<String> handleFormat(ImportFormatException e) {
        return ResponseEntity.status(HttpStatus.BAD_REQUEST)
                .body("Ошибка формата файла: " + e.getMessage());
    }
}
