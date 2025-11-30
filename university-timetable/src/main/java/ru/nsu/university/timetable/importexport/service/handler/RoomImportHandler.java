package ru.nsu.university.timetable.importexport.service.handler;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.domain.Room;
import ru.nsu.university.timetable.domain.Status;
import ru.nsu.university.timetable.importexport.dto.ImportPreviewDto;
import ru.nsu.university.timetable.importexport.dto.ImportResultDto;
import ru.nsu.university.timetable.importexport.dto.ImportRowStatusDto;
import ru.nsu.university.timetable.repo.RoomRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class RoomImportHandler implements ImportHandler {

    private final RoomRepository roomRepository;

    private static final List<String> EXPECTED_HEADERS = List.of(
            "building",
            "number",
            "capacity",
            "status"
    );

    @Override
    public String getType() {
        return "rooms";
    }

    @Override
    public ImportPreviewDto preview(List<Map<String, String>> rows) {
        ImportPreviewDto preview = new ImportPreviewDto();
        preview.setTotalRows(rows.size());
        preview.setExpectedHeaders(EXPECTED_HEADERS);

        List<ImportRowStatusDto> statuses = new ArrayList<>();
        int valid = 0;
        int invalid = 0;
        int rowNumber = 1;

        for (Map<String, String> row : rows) {
            ImportRowStatusDto st = new ImportRowStatusDto();
            st.setRowNumber(rowNumber++);

            List<String> problems = validateRow(row);

            String building = row.getOrDefault("building", "").trim();
            String number = row.getOrDefault("number", "").trim();
            if (!building.isEmpty() && !number.isEmpty()
                    && roomRepository.existsByBuildingAndNumber(building, number)) {
                problems.add("Аудитория " + building + " " + number + " уже существует");
            }

            if (problems.isEmpty()) {
                st.setValid(true);
                st.setHasWarnings(false);
                st.setMessage("OK");
                valid++;
            } else {
                st.setValid(false);
                st.setHasWarnings(false);
                st.setMessage(String.join("; ", problems));
                invalid++;
            }

            statuses.add(st);
        }

        preview.setValidRows(valid);
        preview.setInvalidRows(invalid);
        preview.setRows(statuses);

        if (!rows.isEmpty()) {
            preview.setDetectedHeaders(new ArrayList<>(rows.get(0).keySet()));
        }

        return preview;
    }

    @Override
    public ImportResultDto commit(List<Map<String, String>> rows) {
        ImportResultDto result = new ImportResultDto();
        List<ImportRowStatusDto> statuses = new ArrayList<>();

        int imported = 0;
        int skipped = 0;
        int rowNumber = 1;

        for (Map<String, String> row : rows) {
            ImportRowStatusDto st = new ImportRowStatusDto();
            st.setRowNumber(rowNumber++);

            List<String> problems = validateRow(row);

            String building = row.getOrDefault("building", "").trim();
            String number = row.getOrDefault("number", "").trim();
            if (!building.isEmpty() && !number.isEmpty()
                    && roomRepository.existsByBuildingAndNumber(building, number)) {
                problems.add("Аудитория " + building + " " + number + " уже существует — строка пропущена");
            }

            if (!problems.isEmpty()) {
                st.setValid(false);
                st.setHasWarnings(false);
                st.setMessage(String.join("; ", problems));
                skipped++;
            } else {
                String capacityStr = row.get("capacity").trim();
                int capacity = Integer.parseInt(capacityStr);
                String statusStr = row.getOrDefault("status", "").trim();
                Status status = parseStatusOrDefault(statusStr, Status.ACTIVE);

                Room room = Room.builder()
                        .building(building)
                        .number(number)
                        .capacity(capacity)
                        .status(status)
                        .build();

                roomRepository.save(room);

                st.setValid(true);
                st.setHasWarnings(false);
                st.setMessage("Импортирована");
                imported++;
            }

            statuses.add(st);
        }

        result.setImportedRows(imported);
        result.setSkippedRows(skipped);
        result.setRows(statuses);
        return result;
    }

    private List<String> validateRow(Map<String, String> row) {
        List<String> problems = new ArrayList<>();

        String building = row.getOrDefault("building", "").trim();
        String number = row.getOrDefault("number", "").trim();
        String capacityStr = row.getOrDefault("capacity", "").trim();
        String statusStr = row.getOrDefault("status", "").trim();

        if (building.isEmpty()) problems.add("building обязателен");
        if (number.isEmpty()) problems.add("number обязателен");

        if (capacityStr.isEmpty()) {
            problems.add("capacity обязателен");
        } else {
            try {
                int c = Integer.parseInt(capacityStr);
                if (c < 0) problems.add("capacity не может быть отрицательным");
            } catch (NumberFormatException e) {
                problems.add("capacity должен быть целым числом");
            }
        }

        if (!statusStr.isEmpty()) {
            try {
                Status.valueOf(statusStr.toUpperCase());
            } catch (IllegalArgumentException ex) {
                problems.add("Некорректный status: " + statusStr);
            }
        }

        return problems;
    }

    private Status parseStatusOrDefault(String value, Status defaultStatus) {
        if (value == null || value.isBlank()) {
            return defaultStatus;
        }
        try {
            return Status.valueOf(value.toUpperCase());
        } catch (IllegalArgumentException ex) {
            return defaultStatus;
        }
    }
}
