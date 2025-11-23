package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.*;
import ru.nsu.university.timetable.dto.groups.*;
import ru.nsu.university.timetable.repo.GroupRepository;
import ru.nsu.university.timetable.repo.SubgroupRepository;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class GroupService {
    private final GroupRepository groupRepo;
    private final SubgroupRepository subgroupRepo;

    public GroupResponse create(CreateGroupRequest req) {
        if (groupRepo.existsByCode(req.code()))
            throw new IllegalArgumentException("group code already exists");

        Group g = Group.builder()
                .name(req.name())
                .code(req.code())
                .size(req.size())
                .status(Status.ACTIVE)
                .build();
        return map(groupRepo.save(g));
    }

    public List<GroupResponse> findAll() {
        return groupRepo.findAll().stream().map(this::map).toList();
    }

    public GroupResponse findOne(UUID id) {
        return groupRepo.findById(id).map(this::map).orElseThrow();
    }

    public GroupResponse update(UUID id, UpdateGroupRequest req) {
        Group g = groupRepo.findById(id).orElseThrow();
        if (req.name() != null) g.setName(req.name());
        if (req.code() != null && !req.code().equals(g.getCode())) {
            if (groupRepo.existsByCode(req.code()))
                throw new IllegalArgumentException("group code already exists");
            g.setCode(req.code());
        }
        if (req.size() != null) g.setSize(req.size());

        return map(g);
    }

    public GroupResponse archive(UUID id) {
        Group g = groupRepo.findById(id).orElseThrow();
        g.setStatus(Status.INACTIVE);
        return map(g);
    }

    public SubgroupDto createSubgroup(UUID groupId, CreateSubgroupRequest req) {
        Group group = groupRepo.findById(groupId).orElseThrow();
        if (subgroupRepo.existsByGroupIdAndNameIgnoreCase(groupId, req.name()))
            throw new IllegalArgumentException("subgroup with this name already exists in group");

        Subgroup s = Subgroup.builder()
                .group(group)
                .name(req.name())
                .size(req.size())
                .status(Status.ACTIVE)
                .build();

        s = subgroupRepo.save(s);
        return mapSub(s);
    }

    public SubgroupDto updateSubgroup(UUID id, UpdateSubgroupRequest req) {
        Subgroup s = subgroupRepo.findById(id).orElseThrow();
        if (req.name() != null && !req.name().equalsIgnoreCase(s.getName())) {
            if (subgroupRepo.existsByGroupIdAndNameIgnoreCase(s.getGroup().getId(), req.name()))
                throw new IllegalArgumentException("subgroup with this name already exists in group");
            s.setName(req.name());
        }
        if (req.size() != null) s.setSize(req.size());
        return mapSub(s);
    }

    public SubgroupDto archiveSubgroup(UUID id) {
        Subgroup s = subgroupRepo.findById(id).orElseThrow();
        s.setStatus(Status.INACTIVE);
        return mapSub(s);
    }

    private GroupResponse map(Group g) {
        List<SubgroupDto> subs = g.getSubgroups().stream()
                .map(this::mapSub)
                .toList();

        return new GroupResponse(
                g.getId(),
                g.getName(),
                g.getCode(),
                g.getSize(),
                g.getStatus(),
                g.getCreatedAt(),
                g.getUpdatedAt(),
                subs
        );
    }

    private SubgroupDto mapSub(Subgroup s) {
        return new SubgroupDto(
                s.getId(),
                s.getName(),
                s.getSize(),
                s.getStatus()
        );
    }
}
