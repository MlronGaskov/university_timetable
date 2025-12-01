package ru.nsu.university.timetable.service;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.*;
import ru.nsu.university.timetable.dto.courses.CourseDto;
import ru.nsu.university.timetable.dto.courses.CourseListItemDto;
import ru.nsu.university.timetable.dto.courses.CreateCourseRequest;
import ru.nsu.university.timetable.dto.courses.UpdateCourseRequest;
import ru.nsu.university.timetable.repo.CourseRepository;
import ru.nsu.university.timetable.repo.RoomEquipmentRepository;

import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class CourseServiceImpl implements CourseService {

    private final CourseRepository courseRepository;
    private final RoomEquipmentRepository roomEquipmentRepository;

    @PersistenceContext
    private EntityManager em;

    @Override
    public CourseDto create(CreateCourseRequest request) {
        if (courseRepository.existsByCodeIgnoreCase(request.code())) {
            throw new DuplicateCourseCodeException(request.code());
        }

        Course course;
        if (request.templateCourseId() != null) {
            Course template = courseRepository.findById(request.templateCourseId())
                    .orElseThrow(() -> new CourseNotFoundException(request.templateCourseId()));
            course = fromTemplate(template, request);
        } else {
            course = fromCreateRequest(request);
        }

        course.setStatus(CourseStatus.ACTIVE);

        course = courseRepository.save(course);
        return toDto(course);
    }

    @Override
    @Transactional(readOnly = true)
    public CourseDto getById(UUID id) {
        Course course = courseRepository.findById(id)
                .orElseThrow(() -> new CourseNotFoundException(id));
        return toDto(course);
    }

    @Override
    @Transactional(readOnly = true)
    public List<CourseListItemDto> list(String department, ActivityForm activityForm) {
        return courseRepository.findByFilters(department, activityForm)
                .stream()
                .map(this::toListItemDto)
                .toList();
    }

    @Override
    public CourseDto update(UUID id, UpdateCourseRequest request) {
        Course course = courseRepository.findById(id)
                .orElseThrow(() -> new CourseNotFoundException(id));

        applyUpdate(course, request);

        int scheduledHours = 0; // TODO: интегрировать с расписанием, когда появится
        int newPlanned = course.getTotalPlannedHours();

        if (newPlanned < scheduledHours) {
            throw new CourseHoursConflictException(id, newPlanned, scheduledHours);
        }

        return toDto(course);
    }

    @Override
    public void archiveOrDelete(UUID id, boolean hardDeleteRequested) {
        Course course = courseRepository.findById(id)
                .orElseThrow(() -> new CourseNotFoundException(id));

        boolean hasReferences = false; // TODO: проверить ссылки на курс в расписании/планах

        if (!hardDeleteRequested || hasReferences) {
            course.setStatus(CourseStatus.ARCHIVED);
            return;
        }

        courseRepository.delete(course);
    }

    // --------- mapping helpers ---------

    private Course fromCreateRequest(CreateCourseRequest r) {
        Course course = Course.builder()
                .code(r.code())
                .title(r.title())
                .department(r.department())
                .activityForms(new HashSet<>(r.activityForms()))
                .lectureHours(r.lectureHours())
                .seminarHours(r.seminarHours())
                .labHours(r.labHours())
                .roomRequirements(r.roomRequirements())
                .build();

        if (r.equipmentRequirementIds() != null && !r.equipmentRequirementIds().isEmpty()) {
            course.setEquipmentRequirements(fetchEquipment(r.equipmentRequirementIds()));
        }

        return course;
    }

    private Course fromTemplate(Course template, CreateCourseRequest r) {
        Course.CourseBuilder builder = Course.builder()
                .code(r.code()) // код всегда новый
                .title(Optional.ofNullable(r.title()).filter(t -> !t.isBlank()).orElse(template.getTitle()))
                .department(Optional.ofNullable(r.department()).orElse(template.getDepartment()))
                .activityForms(
                        (r.activityForms() != null && !r.activityForms().isEmpty())
                                ? new HashSet<>(r.activityForms())
                                : new HashSet<>(template.getActivityForms())
                )
                .lectureHours(r.lectureHours() >= 0 ? r.lectureHours() : template.getLectureHours())
                .seminarHours(r.seminarHours() >= 0 ? r.seminarHours() : template.getSeminarHours())
                .labHours(r.labHours() >= 0 ? r.labHours() : template.getLabHours())
                .roomRequirements(
                        r.roomRequirements() != null
                                ? r.roomRequirements()
                                : template.getRoomRequirements()
                );

        Course course = builder.build();

        if (r.equipmentRequirementIds() != null && !r.equipmentRequirementIds().isEmpty()) {
            course.setEquipmentRequirements(fetchEquipment(r.equipmentRequirementIds()));
        } else {
            course.setEquipmentRequirements(
                    template.getEquipmentRequirements() != null
                            ? new HashSet<>(template.getEquipmentRequirements())
                            : Collections.emptySet()
            );
        }

        return course;
    }

    private void applyUpdate(Course course, UpdateCourseRequest r) {
        course.setTitle(r.title());
        course.setDepartment(r.department());
        course.setActivityForms(new HashSet<>(r.activityForms()));
        course.setLectureHours(r.lectureHours());
        course.setSeminarHours(r.seminarHours());
        course.setLabHours(r.labHours());
        course.setRoomRequirements(r.roomRequirements());

        if (r.equipmentRequirementIds() != null) {
            course.setEquipmentRequirements(fetchEquipment(r.equipmentRequirementIds()));
        }
    }

    private Set<RoomEquipment> fetchEquipment(Set<UUID> equipmentIds) {
        if (equipmentIds == null || equipmentIds.isEmpty()) {
            return Collections.emptySet();
        }
        return roomEquipmentRepository.findAllById(equipmentIds)
                .stream()
                .collect(Collectors.toSet());
    }

    private CourseDto toDto(Course c) {
        Set<UUID> equipmentIds =
                c.getEquipmentRequirements() == null
                        ? Collections.emptySet()
                        : c.getEquipmentRequirements().stream()
                        .map(RoomEquipment::getId)
                        .collect(Collectors.toSet());

        return new CourseDto(
                c.getId(),
                c.getCode(),
                c.getTitle(),
                c.getDepartment(),
                c.getActivityForms(),
                c.getLectureHours(),
                c.getSeminarHours(),
                c.getLabHours(),
                c.getRoomRequirements(),
                equipmentIds,
                c.getStatus(),
                c.getCreatedAt(),
                c.getUpdatedAt()
        );
    }

    private CourseListItemDto toListItemDto(Course c) {
        return new CourseListItemDto(
                c.getId(),
                c.getCode(),
                c.getTitle(),
                c.getDepartment(),
                c.getActivityForms(),
                c.getStatus()
        );
    }
}
