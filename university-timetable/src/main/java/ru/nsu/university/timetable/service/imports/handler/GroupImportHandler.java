package ru.nsu.university.timetable.service.imports.handler;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.dto.imports.ImportPreviewDto;
import ru.nsu.university.timetable.dto.imports.ImportResultDto;
import ru.nsu.university.timetable.dto.imports.ImportRowStatusDto;
import ru.nsu.university.timetable.repo.GroupRepository;
import ru.nsu.university.timetable.domain.Group;
import ru.nsu.university.timetable.domain.Status;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

@Component
@RequiredArgsConstructor
public class GroupImportHandler implements ImportHandler {

    private final GroupRepository groupRepository;

    private static final List<String> EXPECTED_HEADERS = List.of(
            "code",
            "name",
            "size",
            "status"
    );

    @Override
    public String getType() {
        return "groups";
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

            String code = row.getOrDefault("code", "").trim();
            if (!code.isEmpty() && groupRepository.existsByCode(code)) {
                problems.add("Группа с таким code уже существует");
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

            String code = row.getOrDefault("code", "").trim();
            if (!code.isEmpty() && groupRepository.existsByCode(code)) {
                problems.add("Группа с таким code уже существует — строка пропущена");
            }

            if (!problems.isEmpty()) {
                st.setValid(false);
                st.setHasWarnings(false);
                st.setMessage(String.join("; ", problems));
                skipped++;
            } else {
                String name = row.get("name").trim();
                int size = Integer.parseInt(row.get("size").trim());
                String statusStr = row.getOrDefault("status", "").trim();
                Status status = parseStatusOrDefault(statusStr, Status.ACTIVE);

                Group group = Group.builder()
                        .code(code)
                        .name(name)
                        .size(size)
                        .status(status)
                        .build();

                groupRepository.save(group);

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

        String code = row.getOrDefault("code", "").trim();
        String name = row.getOrDefault("name", "").trim();
        String sizeStr = row.getOrDefault("size", "").trim();
        String statusStr = row.getOrDefault("status", "").trim();

        if (code.isEmpty()) problems.add("code обязателен");
        if (name.isEmpty()) problems.add("name обязателен");

        if (sizeStr.isEmpty()) {
            problems.add("size обязателен");
        } else {
            try {
                int size = Integer.parseInt(sizeStr);
                if (size < 0) {
                    problems.add("size не может быть отрицательным");
                }
            } catch (NumberFormatException e) {
                problems.add("size должен быть целым числом");
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
