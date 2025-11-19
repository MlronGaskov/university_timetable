package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ru.nsu.university.timetable.schedule.ScheduleVersion;
import ru.nsu.university.timetable.schedule.ScheduleVersionStatus;
import ru.nsu.university.timetable.dto.ScheduleVersionDto;
import ru.nsu.university.timetable.mapper.ScheduleMapper;
import ru.nsu.university.timetable.repo.ScheduleVersionRepository;

import java.time.Instant;
import java.util.List;
import java.util.UUID;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ScheduleVersionService {

    private final ScheduleVersionRepository versionRepository;

    public ScheduleVersionDto archive(UUID versionId) {
        ScheduleVersion version = versionRepository.findById(versionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Version not found"));

        if (version.getStatus() == ScheduleVersionStatus.ARCHIVED) {
            return ScheduleMapper.toDto(version);
        }

        version.setStatus(ScheduleVersionStatus.ARCHIVED);
        version.setUpdatedAt(Instant.now());
        return ScheduleMapper.toDto(versionRepository.save(version));
    }

    public ScheduleVersionDto restore(UUID versionId) {
        ScheduleVersion version = versionRepository.findById(versionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.NOT_FOUND, "Version not found"));

        if (version.getStatus() != ScheduleVersionStatus.ARCHIVED) {
            return ScheduleMapper.toDto(version);
        }

        version.setStatus(ScheduleVersionStatus.DRAFT);
        version.setUpdatedAt(Instant.now());
        return ScheduleMapper.toDto(versionRepository.save(version));
    }

    public List<ScheduleVersionDto> listBySemester(UUID semesterId) {
        return versionRepository.findBySemesterIdOrderByCreatedAtDesc(semesterId)
                .stream()
                .map(ScheduleMapper::toDto)
                .collect(Collectors.toList());
    }
}
