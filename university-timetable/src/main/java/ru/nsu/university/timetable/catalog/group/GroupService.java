package ru.nsu.university.timetable.catalog.group;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.group.dto.CreateGroupRequest;
import ru.nsu.university.timetable.catalog.group.dto.GroupResponse;
import ru.nsu.university.timetable.catalog.group.dto.UpdateGroupRequest;
import ru.nsu.university.timetable.user.student.StudentRepository;
import ru.nsu.university.timetable.web.OptimisticLockingGuard;
import ru.nsu.university.timetable.schedule.timetable.regeneration.ScheduleRegenerationService;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class GroupService {

    private final GroupRepository groupRepository;
    private final StudentRepository studentRepository;
    private final ScheduleRegenerationService scheduleRegenerationService;

    public GroupResponse create(CreateGroupRequest req) {
        if (groupRepository.existsByCodeIgnoreCase(req.code())) {
            throw new IllegalArgumentException(
                    "Group with code '%s' already exists".formatted(req.code())
            );
        }

        Group g = Group.builder()
                .name(req.name())
                .code(req.code())
                .size(req.size())
                .status(Status.ACTIVE)
                .build();

        return map(groupRepository.saveAndFlush(g));
    }

    @Transactional(readOnly = true)
    public List<GroupResponse> findAll() {
        return groupRepository.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public GroupResponse findOne(UUID id) {
        return groupRepository.findById(id)
                .map(this::map)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + id));
    }

    public GroupResponse update(UUID id, long expectedVersion, UpdateGroupRequest req) {
        Group g = findEntityForMutation(id, expectedVersion);

        if (req.name() != null) {
            g.setName(req.name());
        }

        if (req.code() != null && !req.code().equalsIgnoreCase(g.getCode())) {
            if (groupRepository.existsByCodeIgnoreCase(req.code())) {
                throw new IllegalArgumentException(
                        "Group with code '%s' already exists".formatted(req.code())
                );
            }
            g.setCode(req.code());
        }

        if (req.size() != null) {
            g.setSize(req.size());
        }

        Group saved = groupRepository.saveAndFlush(g);
        scheduleRegenerationService.regenerateAfterGroupChanged(saved.getCode());
        return map(saved);
    }

    public GroupResponse archive(UUID id, long expectedVersion) {
        Group g = findEntityForMutation(id, expectedVersion);
        g.setStatus(Status.INACTIVE);
        Group saved = groupRepository.saveAndFlush(g);
        scheduleRegenerationService.regenerateAfterGroupChanged(saved.getCode());
        return map(saved);
    }

    public GroupResponse activate(UUID id, long expectedVersion) {
        Group g = findEntityForMutation(id, expectedVersion);
        g.setStatus(Status.ACTIVE);
        Group saved = groupRepository.saveAndFlush(g);
        scheduleRegenerationService.regenerateAfterGroupChanged(saved.getCode());
        return map(saved);
    }

    public GroupResponse addStudent(UUID groupId, long expectedVersion, String studentId) {
        Group group = findEntityForMutation(groupId, expectedVersion);

        if (studentId == null || studentId.isBlank()) {
            throw new IllegalArgumentException("studentId must not be blank");
        }
        String normalized = studentId.trim();

        if (!studentRepository.existsByStudentId(normalized)) {
            throw new IllegalArgumentException("Student not found: " + normalized);
        }

        if (!group.getStudentIds().contains(normalized)) {
            group.getStudentIds().add(normalized);
        }

        Group saved = groupRepository.saveAndFlush(group);
        scheduleRegenerationService.regenerateAfterGroupChanged(saved.getCode());
        return map(saved);
    }

    public GroupResponse removeStudent(UUID groupId, long expectedVersion, String studentId) {
        Group group = findEntityForMutation(groupId, expectedVersion);

        if (studentId != null && !studentId.isBlank()) {
            String normalized = studentId.trim();
            group.getStudentIds().remove(normalized);
        }

        Group saved = groupRepository.saveAndFlush(group);
        scheduleRegenerationService.regenerateAfterGroupChanged(saved.getCode());
        return map(saved);
    }

    private Group findEntityForMutation(UUID id, long expectedVersion) {
        Group g = groupRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + id));
        OptimisticLockingGuard.verifyVersion("Group", id, expectedVersion, g.getVersion());
        return g;
    }

    private GroupResponse map(Group g) {
        return new GroupResponse(
                g.getId(),
                g.getVersion(),
                g.getName(),
                g.getCode(),
                g.getSize(),
                g.getStatus(),
                g.getCreatedAt(),
                g.getUpdatedAt(),
                List.copyOf(g.getStudentIds())
        );
    }
}
