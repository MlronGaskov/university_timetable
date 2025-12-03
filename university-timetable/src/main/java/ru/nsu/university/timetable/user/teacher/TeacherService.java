package ru.nsu.university.timetable.user.teacher;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.teacher.dto.CreateTeacherRequest;
import ru.nsu.university.timetable.user.teacher.dto.TeacherResponse;
import ru.nsu.university.timetable.user.teacher.dto.UpdateTeacherRequest;
import ru.nsu.university.timetable.user.teacher.dto.UpdateWorkingHoursRequest;
import ru.nsu.university.timetable.user.teacher.dto.WorkingIntervalDto;

import java.time.DayOfWeek;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class TeacherService {
    private final TeacherRepository repo;

    public TeacherResponse create(CreateTeacherRequest req) {
        if (repo.existsByFullNameIgnoreCase(req.fullName())) {
            throw new IllegalArgumentException("teacher already exists");
        }

        Teacher t = Teacher.builder()
                .fullName(req.fullName())
                .status(Status.ACTIVE)
                .build();

        return map(repo.save(t));
    }

    @Transactional(readOnly = true)
    public List<TeacherResponse> findAll() {
        return repo.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public TeacherResponse findOne(UUID id) {
        return repo.findById(id)
                .map(this::map)
                .orElseThrow();
    }

    public TeacherResponse update(UUID id, UpdateTeacherRequest req) {
        Teacher t = repo.findById(id).orElseThrow();

        if (req.fullName() != null && !req.fullName().equalsIgnoreCase(t.getFullName())) {
            if (repo.existsByFullNameIgnoreCase(req.fullName())) {
                throw new IllegalArgumentException("teacher already exists");
            }
            t.setFullName(req.fullName());
        }

        if (req.preferredWorkingHours() != null) {
            Set<TeacherWorkingHours> validated = toEntityWorkingHours(req.preferredWorkingHours());
            t.setPreferredWorkingHours(validated);
        }

        return map(t);
    }

    public TeacherResponse archive(UUID id) {
        Teacher t = repo.findById(id).orElseThrow();
        t.setStatus(Status.INACTIVE);
        return map(t);
    }

    public TeacherResponse activate(UUID id) {
        Teacher t = repo.findById(id).orElseThrow();
        t.setStatus(Status.ACTIVE);
        return map(t);
    }

    public TeacherResponse updateWorkingHours(UUID teacherId, UpdateWorkingHoursRequest req) {
        Teacher t = repo.findById(teacherId).orElseThrow();

        Set<TeacherWorkingHours> validated = toEntityWorkingHours(req.preferredWorkingHours());
        t.setPreferredWorkingHours(validated);

        return map(t);
    }

    private TeacherResponse map(Teacher t) {
        List<WorkingIntervalDto> hours = t.getPreferredWorkingHours().stream()
                .sorted(Comparator
                        .comparing(TeacherWorkingHours::getDay)
                        .thenComparing(TeacherWorkingHours::getStartTime))
                .map(h -> new WorkingIntervalDto(h.getDay(), h.getStartTime(), h.getEndTime()))
                .toList();

        return new TeacherResponse(
                t.getId(),
                t.getFullName(),
                t.getStatus(),
                hours,
                t.getCreatedAt(),
                t.getUpdatedAt()
        );
    }

    private Set<TeacherWorkingHours> toEntityWorkingHours(List<WorkingIntervalDto> dtos) {
        if (dtos == null || dtos.isEmpty()) {
            return Collections.emptySet();
        }

        Map<DayOfWeek, List<WorkingIntervalDto>> byDay = dtos.stream()
                .collect(Collectors.groupingBy(WorkingIntervalDto::day));

        for (var entry : byDay.entrySet()) {
            DayOfWeek day = entry.getKey();
            if (day == DayOfWeek.SUNDAY) {
                throw new IllegalArgumentException("Sunday is not allowed in working hours");
            }

            List<WorkingIntervalDto> intervals = entry.getValue().stream()
                    .sorted(Comparator.comparing(WorkingIntervalDto::startTime))
                    .toList();

            for (int i = 0; i < intervals.size(); i++) {
                WorkingIntervalDto current = intervals.get(i);

                if (!current.startTime().isBefore(current.endTime())) {
                    throw new IllegalArgumentException(
                            "Invalid interval for " + day + ": start must be before end"
                    );
                }

                if (i > 0) {
                    WorkingIntervalDto prev = intervals.get(i - 1);
                    if (prev.endTime().isAfter(current.startTime())) {
                        throw new IllegalArgumentException(
                                "Overlapping intervals for " + day + " are not allowed"
                        );
                    }
                }
            }
        }

        return dtos.stream()
                .map(d -> TeacherWorkingHours.builder()
                        .day(d.day())
                        .startTime(d.startTime())
                        .endTime(d.endTime())
                        .build())
                .collect(Collectors.toCollection(LinkedHashSet::new));
    }
}
