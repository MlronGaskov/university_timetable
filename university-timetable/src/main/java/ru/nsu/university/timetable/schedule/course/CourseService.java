package ru.nsu.university.timetable.schedule.course;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.group.Group;
import ru.nsu.university.timetable.catalog.group.GroupRepository;
import ru.nsu.university.timetable.schedule.course.dto.CourseEquipmentItemDto;
import ru.nsu.university.timetable.schedule.course.dto.CourseResponse;
import ru.nsu.university.timetable.schedule.course.dto.CreateCourseRequest;
import ru.nsu.university.timetable.schedule.course.dto.UpdateCourseRequest;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class CourseService {
    private final CourseRepository courseRepository;
    private final GroupRepository groupRepository;
    private final TeacherRepository teacherRepository;

    public CourseResponse create(CreateCourseRequest req) {
        if (courseRepository.existsByCodeIgnoreCase(req.code())) {
            throw new IllegalArgumentException(
                    "Course with code '%s' already exists".formatted(req.code())
            );
        }

        if (!teacherRepository.existsByTeacherId(req.teacherId())) {
            throw new IllegalArgumentException("Teacher not found: " + req.teacherId());
        }

        List<String> normalizedGroupCodes = normalizeGroupCodes(req.groupCodes());
        for (String groupCode : normalizedGroupCodes) {
            if (!groupRepository.existsByCodeIgnoreCase(groupCode)) {
                throw new IllegalArgumentException("Group not found: " + groupCode);
            }
        }

        Course course = Course.builder()
                .code(req.code())
                .title(req.title())
                .teacherId(req.teacherId().trim())
                .plannedHours(req.plannedHours())
                .status(Status.ACTIVE)
                .groupCodes(new ArrayList<>(normalizedGroupCodes))
                .equipmentRequirements(mapItems(req.equipmentRequirements()))
                .requiredRoomCapacity(0)
                .build();

        // Вычисляем requiredRoomCapacity после установки groupCodes
        course.setRequiredRoomCapacity(calculateRequiredRoomCapacity(course));

        return map(courseRepository.save(course));
    }

    @Transactional(readOnly = true)
    public List<CourseResponse> findAll() {
        return courseRepository.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public CourseResponse findOne(UUID id) {
        return courseRepository.findById(id)
                .map(this::map)
                .orElseThrow(() -> new IllegalArgumentException("Course not found: " + id));
    }

    public CourseResponse update(UUID id, UpdateCourseRequest req) {
        Course course = courseRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Course not found: " + id));

        if (req.code() != null && !req.code().equalsIgnoreCase(course.getCode())) {
            if (courseRepository.existsByCodeIgnoreCase(req.code())) {
                throw new IllegalArgumentException(
                        "Course with code '%s' already exists".formatted(req.code())
                );
            }
            course.setCode(req.code());
        }

        if (req.title() != null) {
            course.setTitle(req.title());
        }

        if (req.teacherId() != null && !req.teacherId().equalsIgnoreCase(course.getTeacherId())) {
            if (!teacherRepository.existsByTeacherId(req.teacherId())) {
                throw new IllegalArgumentException("Teacher not found: " + req.teacherId());
            }
            course.setTeacherId(req.teacherId().trim());
        }

        if (req.plannedHours() != null) {
            course.setPlannedHours(req.plannedHours());
        }

        if (req.groupCodes() != null) {
            List<String> normalizedGroupCodes = normalizeGroupCodes(req.groupCodes());
            for (String groupCode : normalizedGroupCodes) {
                if (!groupRepository.existsByCodeIgnoreCase(groupCode)) {
                    throw new IllegalArgumentException("Group not found: " + groupCode);
                }
            }
            course.setGroupCodes(new ArrayList<>(normalizedGroupCodes));
            // Пересчитываем requiredRoomCapacity при изменении групп
            course.setRequiredRoomCapacity(calculateRequiredRoomCapacity(course));
        }

        if (req.equipmentRequirements() != null) {
            course.setEquipmentRequirements(mapItems(req.equipmentRequirements()));
        }

        return map(course);
    }

    public CourseResponse archive(UUID id) {
        Course course = courseRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Course not found: " + id));

        course.setStatus(Status.INACTIVE);
        return map(course);
    }

    public CourseResponse activate(UUID id) {
        Course course = courseRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Course not found: " + id));

        course.setStatus(Status.ACTIVE);
        return map(course);
    }

    public CourseResponse addGroup(UUID courseId, String groupCode) {
        Course course = courseRepository.findById(courseId)
                .orElseThrow(() -> new IllegalArgumentException("Course not found: " + courseId));

        if (groupCode == null || groupCode.isBlank()) {
            throw new IllegalArgumentException("groupCode must not be blank");
        }
        String normalized = groupCode.trim();

        if (!groupRepository.existsByCodeIgnoreCase(normalized)) {
            throw new IllegalArgumentException("Group not found: " + normalized);
        }

        if (!course.getGroupCodes().contains(normalized)) {
            course.getGroupCodes().add(normalized);
            // Пересчитываем requiredRoomCapacity при добавлении группы
            course.setRequiredRoomCapacity(calculateRequiredRoomCapacity(course));
        }

        return map(course);
    }

    public CourseResponse removeGroup(UUID courseId, String groupCode) {
        Course course = courseRepository.findById(courseId)
                .orElseThrow(() -> new IllegalArgumentException("Course not found: " + courseId));

        if (groupCode != null && !groupCode.isBlank()) {
            String normalized = groupCode.trim();
            course.getGroupCodes().remove(normalized);
            // Пересчитываем requiredRoomCapacity при удалении группы
            course.setRequiredRoomCapacity(calculateRequiredRoomCapacity(course));
        }

        return map(course);
    }

    private List<Course.EquipmentRequirement> mapItems(List<CourseEquipmentItemDto> dtos) {
        List<Course.EquipmentRequirement> result = new ArrayList<>();
        if (dtos == null) {
            return result;
        }

        for (CourseEquipmentItemDto dto : dtos) {
            String normalizedName = dto.name() == null ? "" : dto.name().trim().toLowerCase();
            if (normalizedName.isEmpty()) {
                throw new IllegalArgumentException("Equipment item name must not be blank");
            }

            result.add(new Course.EquipmentRequirement(
                    normalizedName,
                    dto.quantity()
            ));
        }

        return result;
    }

    private CourseResponse map(Course c) {
        List<CourseEquipmentItemDto> items = c.getEquipmentRequirements().stream()
                .map(it -> new CourseEquipmentItemDto(it.getName(), it.getQuantity()))
                .toList();

        return new CourseResponse(
                c.getId(),
                c.getCode(),
                c.getTitle(),
                c.getStatus(),
                c.getPlannedHours(),
                c.getRequiredRoomCapacity(),
                c.getTeacherId(),
                List.copyOf(c.getGroupCodes()),
                items,
                c.getCreatedAt(),
                c.getUpdatedAt()
        );
    }

    private int calculateRequiredRoomCapacity(Course course) {
        if (course.getGroupCodes() == null || course.getGroupCodes().isEmpty()) {
            return 0;
        }

        List<Group> groups = groupRepository.findByCodeIn(course.getGroupCodes());

        int totalSize = groups.stream()
                .mapToInt(Group::getSize)
                .sum();

        return totalSize + 5;
    }

    private List<String> normalizeGroupCodes(List<String> list) {
        if (list == null) return new ArrayList<>();
        List<String> res = new ArrayList<>();
        for (String v : list) {
            if (v == null) continue;
            String trimmed = v.trim();
            if (!trimmed.isEmpty()) {
                res.add(trimmed);
            }
        }
        return res;
    }
}
