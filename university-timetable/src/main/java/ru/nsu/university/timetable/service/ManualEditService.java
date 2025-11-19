package ru.nsu.university.timetable.service;

import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ru.nsu.university.timetable.schedule.ClassSession;
//import ru.nsu.university.timetable.schedule.Course;
//import ru.nsu.university.timetable.schedule.Room;
//import ru.nsu.university.timetable.schedule.Teacher;
import ru.nsu.university.timetable.dto.AssistedPlacementRequest;
import ru.nsu.university.timetable.dto.AssistedPlacementResponse;
import ru.nsu.university.timetable.dto.ClassSessionDto;
import ru.nsu.university.timetable.dto.PlacementOptionDto;
import ru.nsu.university.timetable.dto.UpdateSessionRequest;
import ru.nsu.university.timetable.mapper.ScheduleMapper;
import ru.nsu.university.timetable.repo.ClassSessionRepository;
import ru.nsu.university.timetable.solver.PlacementSuggestionCommand;
import ru.nsu.university.timetable.solver.SchedulerClient;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ManualEditService {

    private final ClassSessionRepository sessionRepository;
    private final ScheduleLockService lockService;
    private final SchedulerClient schedulerClient;
    private final EntityManager entityManager;

    public ClassSessionDto updateSession(UUID versionId, UUID sessionId, UpdateSessionRequest request) {
        lockService.ensureHasLock(versionId);

        ClassSession session = sessionRepository.findById(sessionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Session not found"));

        // простая проверка на конфликты
        List<ClassSession> conflicts = sessionRepository.findConflicts(
                versionId,
                sessionId,
                request.getDate(),
                request.getSlotIndex(),
                request.getTeacherId(),
                session.getGroup().getId(),
                request.getRoomId()
        );
        if (!conflicts.isEmpty()) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "Placement conflicts with existing sessions");
        }

        session.setTeacher(entityManager.getReference(Teacher.class, request.getTeacherId()));
        session.setRoom(entityManager.getReference(Room.class, request.getRoomId()));
        session.setDate(request.getDate());
        session.setSlotIndex(request.getSlotIndex());

        session = sessionRepository.save(session);
        return ScheduleMapper.toDto(session);
    }

    public AssistedPlacementResponse assistedPlacement(UUID versionId, AssistedPlacementRequest request) {
        lockService.ensureHasLock(versionId);

        ClassSession session = sessionRepository.findById(request.getSessionId())
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Session not found"));

        PlacementSuggestionCommand cmd = PlacementSuggestionCommand.of(session, request.getTopK());
        List<PlacementOptionDto> options = schedulerClient.suggestPlacements(cmd);

        AssistedPlacementResponse resp = new AssistedPlacementResponse();
        resp.setOptions(options);
        return resp;
    }
}
