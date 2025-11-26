package ru.nsu.university.timetable.service;

import jakarta.persistence.EntityManager;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ru.nsu.university.timetable.schedule.*;
import ru.nsu.university.timetable.dto.GenerateScheduleRequest;
import ru.nsu.university.timetable.dto.GenerateScheduleResponse;
import ru.nsu.university.timetable.dto.GeneratedAlternativeDto;
import ru.nsu.university.timetable.dto.GeneratedSessionDto;
import ru.nsu.university.timetable.dto.ScheduleVersionDto;
import ru.nsu.university.timetable.mapper.ScheduleMapper;
import ru.nsu.university.timetable.repo.ClassSessionRepository;
import ru.nsu.university.timetable.repo.ScheduleVersionRepository;
import ru.nsu.university.timetable.solver.GenerateScheduleCommand;
import ru.nsu.university.timetable.solver.SchedulerClient;
import ru.nsu.university.timetable.solver.SchedulerGenerationResult;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ScheduleGenerationService {

    private final ScheduleVersionRepository versionRepository;
    private final ClassSessionRepository sessionRepository;
    private final SchedulerClient schedulerClient;
    private final EntityManager entityManager;

    // Предполагается наличие соответствующего репозитория
    private final SemesterRepository semesterRepository;

    public GenerateScheduleResponse generate(GenerateScheduleRequest request) {
        if (request.getSemesterId() == null) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, "semesterId is required");
        }

        UUID semesterId = request.getSemesterId();
        Semester semester = semesterRepository.findById(semesterId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Semester not found"));

        GenerateScheduleCommand command = GenerateScheduleCommand.of(semester, request);
        SchedulerGenerationResult result = schedulerClient.generateFull(command);

        GenerateScheduleResponse response = new GenerateScheduleResponse();

        if (!result.isFeasible()) {
            response.setMus(result.getMus());
            return response;
        }

        List<ScheduleVersionDto> created = new ArrayList<>();

        for (GeneratedAlternativeDto alternative : result.getAlternatives()) {
            ScheduleVersion version = new ScheduleVersion();
            version.setName(request.getName());
            version.setSemester(semester);
            version.setStatus(ScheduleVersionStatus.DRAFT);
            version.setQualityMetrics(alternative.getQuality());
            version.setCreatedAt(Instant.now());

            version = versionRepository.save(version);

            for (GeneratedSessionDto s : alternative.getSessions()) {
                ClassSession session = new ClassSession();
                session.setVersion(version);
                session.setCourse(entityManager.getReference(Course.class, s.getCourseId()));
                session.setTeacher(entityManager.getReference(Teacher.class, s.getTeacherId()));
                session.setGroup(entityManager.getReference(Group.class, s.getGroupId()));
                if (s.getSubgroupId() != null) {
                    session.setSubgroup(entityManager.getReference(Subgroup.class, s.getSubgroupId()));
                }
                session.setRoom(entityManager.getReference(Room.class, s.getRoomId()));
                session.setDate(s.getDate());
                session.setSlotIndex(s.getSlotIndex());
                session.setDurationSlots(1);
                sessionRepository.save(session);
            }

            created.add(ScheduleMapper.toDto(version));
        }

        response.setCreatedVersions(created);
        return response;
    }
}
