package ru.nsu.university.timetable.importexport.service.handler;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.domain.Group;
import ru.nsu.university.timetable.domain.Status;
import ru.nsu.university.timetable.domain.Student;
import ru.nsu.university.timetable.importexport.dto.ImportPreviewDto;
import ru.nsu.university.timetable.importexport.dto.ImportResultDto;
import ru.nsu.university.timetable.importexport.dto.ImportRowStatusDto;
import ru.nsu.university.timetable.repo.GroupRepository;
import ru.nsu.university.timetable.repo.StudentRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Component
@RequiredArgsConstructor
public class StudentImportHandler implements ImportHandler {

    private final StudentRepository studentRepository;
    private final GroupRepository groupRepository;

    private static final List<String> EXPECTED_HEADERS = List.of(
            "full_name",
            "student_id",
            "group_code",
            "status"
    );

    @Override
    public String getType() {
        return "students";
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
            List<String> warnings = new ArrayList<>();

            String studentId = row.getOrDefault("student_id", "").trim();
            if (!studentId.isEmpty() && studentRepository.existsByStudentId(studentId)) {
                warnings.add("student_id уже существует — будет пропущен при импорте");
            }

            if (problems.isEmpty()) {
                st.setValid(warnings.isEmpty());
                st.setHasWarnings(!warnings.isEmpty());
                String base = warnings.isEmpty() ? "OK" : "OK (но с предупреждениями)";
                warnings.add(0, base);
                st.setMessage(String.join("; ", warnings));
                if (warnings.isEmpty()) valid++;
                else invalid++;
            } else {
                problems.addAll(warnings);
                st.setValid(false);
                st.setHasWarnings(!warnings.isEmpty());
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

            String studentId = row.getOrDefault("student_id", "").trim();
            if (!studentId.isEmpty() && studentRepository.existsByStudentId(studentId)) {
                problems.add("student_id уже существует — строка пропущена");
            }

            if (!problems.isEmpty()) {
                st.setValid(false);
                st.setHasWarnings(false);
                st.setMessage(String.join("; ", problems));
                skipped++;
            } else {
                String fullName = row.get("full_name").trim();
                String groupCode = row.get("group_code").trim();
                String statusStr = row.getOrDefault("status", "").trim();

                Optional<Group> groupOpt = groupRepository.findByCode(groupCode);
                Group group = groupOpt.orElseThrow(); // validateRow уже проверил, что группа есть

                Status status = parseStatusOrDefault(statusStr, Status.ACTIVE);

                Student student = Student.builder()
                        .fullName(fullName)
                        .studentId(studentId)
                        .group(group)
                        .status(status)
                        .build();

                studentRepository.save(student);

                st.setValid(true);
                st.setHasWarnings(false);
                st.setMessage("Импортирован");
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

        String fullName = row.getOrDefault("full_name", "").trim();
        String studentId = row.getOrDefault("student_id", "").trim();
        String groupCode = row.getOrDefault("group_code", "").trim();
        String statusStr = row.getOrDefault("status", "").trim();

        if (fullName.isEmpty()) {
            problems.add("full_name обязателен");
        }
        if (studentId.isEmpty()) {
            problems.add("student_id обязателен");
        }
        if (groupCode.isEmpty()) {
            problems.add("group_code обязателен");
        } else {
            if (groupRepository.findByCode(groupCode).isEmpty()) {
                problems.add("Группа с code='" + groupCode + "' не найдена");
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
