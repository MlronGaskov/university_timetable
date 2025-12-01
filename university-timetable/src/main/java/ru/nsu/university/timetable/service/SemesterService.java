package ru.nsu.university.timetable.service;

import java.util.List;
import java.util.UUID;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import ru.nsu.university.timetable.domain.Semester;
import ru.nsu.university.timetable.domain.SemesterStatus;
import ru.nsu.university.timetable.dto.semesters.CreateSemesterRequest;
import ru.nsu.university.timetable.dto.semesters.SemesterResponse;
import ru.nsu.university.timetable.dto.semesters.UpdateSemesterRequest;
import ru.nsu.university.timetable.exception.semester.DuplicateSemesterCodeException;
import ru.nsu.university.timetable.exception.semester.InvalidSemesterDatesException;
import ru.nsu.university.timetable.exception.semester.SemesterDeleteForbiddenException;
import ru.nsu.university.timetable.exception.semester.SemesterNotFoundException;
import ru.nsu.university.timetable.repo.SemesterRepository;


@Service
@RequiredArgsConstructor
@Transactional
public class SemesterService {

    private final SemesterRepository semesterRepository;

    public SemesterResponse create(CreateSemesterRequest request) {
        if (semesterRepository.existsByCodeIgnoreCase(request.code())) {
            throw new DuplicateSemesterCodeException(request.code());
        }

        if (!request.end().isAfter(request.start())) {
            throw new InvalidSemesterDatesException();
        }

        Semester semester = Semester.builder()
                .code(request.code())
                .start(request.start())
                .end(request.end())
                .status(SemesterStatus.ACTIVE)
                .build();

        semester = semesterRepository.save(semester);
        return toDto(semester);
    }

    @Transactional(readOnly = true)
    public SemesterResponse getById(UUID id) {
        Semester semester = semesterRepository.findById(id)
                .orElseThrow(() -> new SemesterNotFoundException(id));
        return toDto(semester);
    }

    @Transactional(readOnly = true)
    public List<SemesterResponse> list(SemesterStatus status) {
        return semesterRepository.findByFilters(status)
                .stream()
                .map(this::toDto)
                .toList();
    }

    public SemesterResponse update(UUID id, UpdateSemesterRequest request) {
        Semester semester = semesterRepository.findById(id)
                .orElseThrow(() -> new SemesterNotFoundException(id));

        semesterRepository.findByCodeIgnoreCase(request.code())
                .ifPresent(existing -> {
                    if (!existing.getId().equals(id)) {
                        throw new DuplicateSemesterCodeException(request.code());
                    }
                });

        if (!request.end().isAfter(request.start())) {
            throw new InvalidSemesterDatesException();
        }

        semester.setCode(request.code());
        semester.setStart(request.start());
        semester.setEnd(request.end());

        return toDto(semester);
    }

    public void archiveOrDelete(UUID id, boolean hardDeleteRequested) {
        Semester semester = semesterRepository.findById(id)
                .orElseThrow(() -> new SemesterNotFoundException(id));

        boolean hasReferences = (semester.getCourses() != null && !semester.getCourses().isEmpty())
                || (semester.getGroups() != null && !semester.getGroups().isEmpty());

        if (!hardDeleteRequested || hasReferences) {
            if (hasReferences && hardDeleteRequested) {
                throw new SemesterDeleteForbiddenException(id);
            }
            semester.setStatus(SemesterStatus.ARCHIVED);
            return;
        }

        semesterRepository.delete(semester);
    }

    private SemesterResponse toDto(Semester s) {
        return new SemesterResponse(
                s.getId(),
                s.getCode(),
                s.getStart(),
                s.getEnd(),
                s.getStatus(),
                s.getCreatedAt(),
                s.getUpdatedAt()
        );
    }
}

