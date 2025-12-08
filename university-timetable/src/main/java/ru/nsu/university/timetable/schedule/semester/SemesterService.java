package ru.nsu.university.timetable.schedule.semester;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.schedule.semester.dto.CreateSemesterRequest;
import ru.nsu.university.timetable.schedule.semester.dto.SemesterResponse;
import ru.nsu.university.timetable.schedule.semester.dto.UpdateSemesterRequest;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class SemesterService {

    private final SemesterRepository semesterRepository;

    public SemesterResponse create(CreateSemesterRequest req) {
        if (semesterRepository.existsByCodeIgnoreCase(req.code())) {
            throw new IllegalArgumentException(
                    "Semester with code '%s' already exists".formatted(req.code())
            );
        }

        Semester semester = Semester.builder()
                .code(req.code().trim())
                .startAt(req.startAt())
                .endAt(req.endAt())
                .status(Status.ACTIVE)
                .policyName(req.policyName().trim())
                .courseCodes(normalizeList(req.courseCodes()))
                .roomCodes(normalizeList(req.roomCodes()))
                .build();

        return map(semesterRepository.save(semester));
    }

    @Transactional(readOnly = true)
    public List<SemesterResponse> findAll() {
        return semesterRepository.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public SemesterResponse findOne(UUID id) {
        Semester s = semesterRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + id));
        return map(s);
    }

    public SemesterResponse update(UUID id, UpdateSemesterRequest req) {
        Semester semester = semesterRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + id));

        if (req.code() != null && !req.code().equalsIgnoreCase(semester.getCode())) {
            if (semesterRepository.existsByCodeIgnoreCase(req.code())) {
                throw new IllegalArgumentException(
                        "Semester with code '%s' already exists".formatted(req.code())
                );
            }
            semester.setCode(req.code().trim());
        }

        if (req.startAt() != null) {
            semester.setStartAt(req.startAt());
        }

        if (req.endAt() != null) {
            semester.setEndAt(req.endAt());
        }

        if (req.policyName() != null) {
            semester.setPolicyName(req.policyName().trim());
        }

        if (req.courseCodes() != null) {
            semester.setCourseCodes(normalizeList(req.courseCodes()));
        }

        if (req.roomCodes() != null) {
            semester.setRoomCodes(normalizeList(req.roomCodes()));
        }

        return map(semester);
    }

    public SemesterResponse archive(UUID id) {
        Semester semester = semesterRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + id));
        semester.setStatus(Status.INACTIVE);
        return map(semester);
    }

    public SemesterResponse activate(UUID id) {
        Semester semester = semesterRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + id));
        semester.setStatus(Status.ACTIVE);
        return map(semester);
    }

    private List<String> normalizeList(List<String> list) {
        if (list == null) return new ArrayList<>();
        List<String> res = new ArrayList<>();
        for (String v : list) {
            if (v == null) continue;
            String trimmed = v.trim();
            if (!trimmed.isEmpty()) {
                res.add(trimmed);
            }
        }
        return res;
    }

    private SemesterResponse map(Semester s) {
        return new SemesterResponse(
                s.getId(),
                s.getCode(),
                s.getStartAt(),
                s.getEndAt(),
                s.getStatus(),
                s.getPolicyName(),
                List.copyOf(s.getCourseCodes()),
                List.copyOf(s.getRoomCodes()),
                s.getCreatedAt(),
                s.getUpdatedAt()
        );
    }
}
