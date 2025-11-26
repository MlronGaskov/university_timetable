package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ru.nsu.university.timetable.schedule.ClassSession;
import ru.nsu.university.timetable.schedule.ScheduleVersion;
import ru.nsu.university.timetable.dto.*;
import ru.nsu.university.timetable.mapper.ScheduleMapper;
import ru.nsu.university.timetable.repo.ClassSessionRepository;
import ru.nsu.university.timetable.repo.ScheduleVersionRepository;
import ru.nsu.university.timetable.solver.LocalRecomputeCommand;
import ru.nsu.university.timetable.solver.SchedulerClient;
import ru.nsu.university.timetable.solver.SchedulerLocalResult;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class LocalRecomputeService {

    private final ScheduleVersionRepository versionRepository;
    private final ClassSessionRepository sessionRepository;
    private final SchedulerClient schedulerClient;

    public Object localRecompute(LocalRecomputeRequest request) {
        if (request.getVersionId() == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "versionId is required");
        }

        UUID versionId = request.getVersionId();
        ScheduleVersion version = versionRepository.findById(versionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Version not found"));

        List<ClassSession> scopeSessions = findScopeSessions(request, version);

        LocalRecomputeCommand command = LocalRecomputeCommand.of(version, scopeSessions, request);
        SchedulerLocalResult result = schedulerClient.localRecompute(command);

        if (!result.isFeasible()) {
            LocalRecomputePreviewResponse resp = new LocalRecomputePreviewResponse();
            resp.setMus(result.getMus());
            resp.setSessionsToChange(scopeSessions.stream()
                    .map(ScheduleMapper::toDto)
                    .collect(Collectors.toList()));
            return resp;
        }

        if (request.isDryRun()) {
            LocalRecomputePreviewResponse resp = new LocalRecomputePreviewResponse();
            resp.setSessionsToChange(scopeSessions.stream()
                    .map(ScheduleMapper::toDto)
                    .collect(Collectors.toList()));

            List<ClassSessionDto> proposed = result.getNewSessions().stream().map(g -> {
                ClassSessionDto dto = new ClassSessionDto();
                dto.setCourseId(g.getCourseId());
                dto.setTeacherId(g.getTeacherId());
                dto.setGroupId(g.getGroupId());
                dto.setSubgroupId(g.getSubgroupId());
                dto.setRoomId(g.getRoomId());
                dto.setDate(g.getDate());
                dto.setSlotIndex(g.getSlotIndex());
                dto.setDurationSlots(1);
                return dto;
            }).collect(Collectors.toList());

            resp.setProposedChanges(proposed);
            return resp;
        }

        // apply changes
        sessionRepository.deleteAll(scopeSessions);
        // фактическое создание новых ClassSession можно делать тут,
        // аналогично генерации (через EntityManager)

        LocalRecomputeApplyResponse resp = new LocalRecomputeApplyResponse();
        resp.setUpdatedVersion(ScheduleMapper.toDto(version));
        return resp;
    }

    private List<ClassSession> findScopeSessions(LocalRecomputeRequest request, ScheduleVersion version) {
        List<ClassSession> all = sessionRepository.findByVersionId(version.getId());

        return all.stream()
                .filter(s -> request.getGroupId() == null || s.getGroup().getId().equals(request.getGroupId()))
                .filter(s -> request.getTeacherId() == null || s.getTeacher().getId().equals(request.getTeacherId()))
                .filter(s -> request.getRoomId() == null || s.getRoom().getId().equals(request.getRoomId()))
                .filter(s -> !s.isLocked())
                .filter(s -> request.getFromDate() == null || !s.getDate().isBefore(request.getFromDate()))
                .filter(s -> request.getToDate() == null || !s.getDate().isAfter(request.getToDate()))
                .toList();
    }
}
