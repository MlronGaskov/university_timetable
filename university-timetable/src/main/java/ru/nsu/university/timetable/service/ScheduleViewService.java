package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import ru.nsu.university.timetable.schedule.ClassSession;
import ru.nsu.university.timetable.dto.ScheduleSliceRequest;
import ru.nsu.university.timetable.dto.ScheduleSliceResponse;
import ru.nsu.university.timetable.dto.SliceType;
import ru.nsu.university.timetable.mapper.ScheduleMapper;
import ru.nsu.university.timetable.repo.ClassSessionRepository;

import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ScheduleViewService {

    private final ClassSessionRepository sessionRepository;

    public ScheduleSliceResponse getSlice(UUID versionId, ScheduleSliceRequest request) {
        List<ClassSession> sessions;

        SliceType type = request.getType();
        UUID entityId = request.getEntityId();

        switch (type) {
            case GROUP -> sessions = sessionRepository.findByVersionIdAndGroupId(versionId, entityId);
            case TEACHER -> sessions = sessionRepository.findByVersionIdAndTeacherId(versionId, entityId);
            case ROOM -> sessions = sessionRepository.findByVersionIdAndRoomId(versionId, entityId);
            default -> throw new IllegalArgumentException("Unknown slice type: " + type);
        }

        if (request.getFromDate() != null) {
            sessions = sessions.stream()
                    .filter(s -> !s.getDate().isBefore(request.getFromDate()))
                    .toList();
        }

        if (request.getToDate() != null) {
            sessions = sessions.stream()
                    .filter(s -> !s.getDate().isAfter(request.getToDate()))
                    .toList();
        }

        ScheduleSliceResponse response = new ScheduleSliceResponse();
        response.setSessions(sessions.stream()
                .map(ScheduleMapper::toDto)
                .collect(Collectors.toList()));
        return response;
    }
}
