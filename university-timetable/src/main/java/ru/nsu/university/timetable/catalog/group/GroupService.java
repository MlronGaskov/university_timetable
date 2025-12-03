package ru.nsu.university.timetable.catalog.group;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.group.dto.CreateGroupRequest;
import ru.nsu.university.timetable.catalog.group.dto.GroupResponse;
import ru.nsu.university.timetable.catalog.group.dto.UpdateGroupRequest;
import ru.nsu.university.timetable.user.student.StudentRepository;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class GroupService {

    private final GroupRepository groupRepository;
    private final StudentRepository studentRepository;

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

        return map(groupRepository.save(g));
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

    public GroupResponse update(UUID id, UpdateGroupRequest req) {
        Group g = groupRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + id));

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

        return map(g);
    }

    public GroupResponse archive(UUID id) {
        Group g = groupRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + id));

        g.setStatus(Status.INACTIVE);
        return map(g);
    }

    public GroupResponse activate(UUID id) {
        Group g = groupRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + id));

        g.setStatus(Status.ACTIVE);
        return map(g);
    }

    public GroupResponse addStudent(UUID groupId, UUID studentId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + groupId));

        if (!studentRepository.existsById(studentId)) {
            throw new IllegalArgumentException("Student not found: " + studentId);
        }

        if (!group.getStudentIds().contains(studentId)) {
            group.getStudentIds().add(studentId);
        }

        return map(group);
    }

    public GroupResponse removeStudent(UUID groupId, UUID studentId) {
        Group group = groupRepository.findById(groupId)
                .orElseThrow(() -> new IllegalArgumentException("Group not found: " + groupId));

        group.getStudentIds().remove(studentId);

        return map(group);
    }

    private GroupResponse map(Group g) {
        return new GroupResponse(
                g.getId(),
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
