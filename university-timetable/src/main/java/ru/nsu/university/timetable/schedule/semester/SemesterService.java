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
                .code(req.code())
                .startAt(req.startAt())
                .endAt(req.endAt())
                .policyId(req.policyId())
                .status(Status.ACTIVE)
                .courseIds(req.courseIds() == null
                        ? new ArrayList<>()
                        : new ArrayList<>(req.courseIds()))
                .roomCodes(req.roomCodes() == null
                        ? new ArrayList<>()
                        : new ArrayList<>(req.roomCodes()))
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
        return semesterRepository.findById(id)
                .map(this::map)
                .orElseThrow(() -> new IllegalArgumentException("Semester not found: " + id));
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
            semester.setCode(req.code());
        }

        if (req.startAt() != null) {
            semester.setStartAt(req.startAt());
        }

        if (req.endAt() != null) {
            semester.setEndAt(req.endAt());
        }

        if (req.policyId() != null) {
            semester.setPolicyId(req.policyId());
        }

        if (req.courseIds() != null) {
            semester.setCourseIds(new ArrayList<>(req.courseIds()));
        }

        if (req.roomCodes() != null) {
            semester.setRoomCodes(new ArrayList<>(req.roomCodes()));
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

    private SemesterResponse map(Semester s) {
        return new SemesterResponse(
                s.getId(),
                s.getCode(),
                s.getStartAt(),
                s.getEndAt(),
                s.getStatus(),
                s.getPolicyId(),
                List.copyOf(s.getCourseIds()),
                List.copyOf(s.getRoomCodes()),
                s.getCreatedAt(),
                s.getUpdatedAt()
        );
    }
}
