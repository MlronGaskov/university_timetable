package ru.nsu.university.timetable.user.teacher;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.teacher.dto.*;
import ru.nsu.university.timetable.web.OptimisticLockingGuard;
import ru.nsu.university.timetable.schedule.timetable.regeneration.ScheduleRegenerationService;
import ru.nsu.university.timetable.web.RetriableTransactionExecutor;

import java.time.DayOfWeek;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class TeacherService {
    private final TeacherRepository repo;
    private final ScheduleRegenerationService scheduleRegenerationService;
    private final RetriableTransactionExecutor transactionExecutor;

    public TeacherResponse create(CreateTeacherRequest req) {
        return transactionExecutor.execute("teacher.create", () -> {
            if (repo.existsByFullNameIgnoreCase(req.fullName())) {
                throw new IllegalArgumentException("teacher already exists");
            }
            if (repo.existsByTeacherId(req.teacherId())) {
                throw new IllegalArgumentException("teacherId already exists");
            }

            Teacher t = Teacher.builder()
                    .teacherId(req.teacherId().trim())
                    .fullName(req.fullName())
                    .status(Status.ACTIVE)
                    .build();

            return map(repo.saveAndFlush(t));
        });
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
                .orElseThrow(() -> new IllegalArgumentException("Teacher not found: " + id));
    }

    public TeacherResponse update(UUID id, long expectedVersion, UpdateTeacherRequest req) {
        return transactionExecutor.execute("teacher.update", () -> {
            Teacher t = findEntityForMutation(id, expectedVersion);

            if (req.teacherId() != null && !req.teacherId().equalsIgnoreCase(t.getTeacherId())) {
                if (repo.existsByTeacherId(req.teacherId())) {
                    throw new IllegalArgumentException("teacherId already exists");
                }
                t.setTeacherId(req.teacherId().trim());
            }

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

            Teacher saved = repo.saveAndFlush(t);
            scheduleRegenerationService.regenerateAfterTeacherWorkingHoursChanged(saved.getTeacherId());
            return map(saved);
        });
    }

    public TeacherResponse archive(UUID id, long expectedVersion) {
        return transactionExecutor.execute("teacher.archive", () -> {
            Teacher t = findEntityForMutation(id, expectedVersion);
            t.setStatus(Status.INACTIVE);
            Teacher saved = repo.saveAndFlush(t);
            scheduleRegenerationService.regenerateAfterTeacherWorkingHoursChanged(saved.getTeacherId());
            return map(saved);
        });
    }

    public TeacherResponse activate(UUID id, long expectedVersion) {
        return transactionExecutor.execute("teacher.activate", () -> {
            Teacher t = findEntityForMutation(id, expectedVersion);
            t.setStatus(Status.ACTIVE);
            Teacher saved = repo.saveAndFlush(t);
            scheduleRegenerationService.regenerateAfterTeacherWorkingHoursChanged(saved.getTeacherId());
            return map(saved);
        });
    }

    public TeacherResponse updateWorkingHours(UUID teacherId, long expectedVersion, UpdateWorkingHoursRequest req) {
        return transactionExecutor.execute("teacher.updateWorkingHours", () -> {
            Teacher t = findEntityForMutation(teacherId, expectedVersion);
            t.setPreferredWorkingHours(toEntityWorkingHours(req.preferredWorkingHours()));
            Teacher saved = repo.saveAndFlush(t);
            scheduleRegenerationService.regenerateAfterTeacherWorkingHoursChanged(saved.getTeacherId());
            return map(saved);
        });
    }

    private Teacher findEntityForMutation(UUID id, long expectedVersion) {
        Teacher t = repo.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Teacher not found: " + id));
        OptimisticLockingGuard.verifyVersion("Teacher", id, expectedVersion, t.getVersion());
        return t;
    }

    private TeacherResponse map(Teacher t) {
        List<WorkingIntervalDto> wh = t.getPreferredWorkingHours().stream()
                .sorted(Comparator
                        .comparing(TeacherWorkingHours::getDay)
                        .thenComparing(TeacherWorkingHours::getStartTime))
                .map(w -> new WorkingIntervalDto(w.getDay(), w.getStartTime(), w.getEndTime()))
                .toList();

        return new TeacherResponse(
                t.getId(),
                t.getVersion(),
                t.getTeacherId(),
                t.getFullName(),
                t.getStatus(),
                wh,
                t.getCreatedAt(),
                t.getUpdatedAt()
        );
    }

    private Set<TeacherWorkingHours> toEntityWorkingHours(List<WorkingIntervalDto> dtos) {
        if (dtos == null) {
            return new LinkedHashSet<>();
        }

        for (WorkingIntervalDto dto : dtos) {
            validateInterval(dto);
        }

        return dtos.stream()
                .map(d -> TeacherWorkingHours.builder()
                        .day(d.day())
                        .startTime(d.startTime())
                        .endTime(d.endTime())
                        .build())
                .collect(Collectors.toCollection(LinkedHashSet::new));
    }

    private void validateInterval(WorkingIntervalDto dto) {
        if (dto.day() == null) {
            throw new IllegalArgumentException("day must not be null");
        }
        if (dto.startTime() == null || dto.endTime() == null) {
            throw new IllegalArgumentException("startTime and endTime must not be null");
        }
        if (!dto.startTime().isBefore(dto.endTime())) {
            throw new IllegalArgumentException("startTime must be before endTime");
        }
        DayOfWeek ignored = dto.day();
    }
}
