package ru.nsu.university.timetable.schedule.timetable.regeneration;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.room.Room;
import ru.nsu.university.timetable.catalog.room.RoomRepository;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;
import ru.nsu.university.timetable.user.teacher.Teacher;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;
import ru.nsu.university.timetable.user.teacher.TeacherWorkingHours;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class ScheduleSlotValidityChecker {
    private final CourseRepository courseRepository;
    private final RoomRepository roomRepository;
    private final TeacherRepository teacherRepository;

    public SlotValidationResult validate(Semester semester, ScheduleSlot slot) {
        if (!containsIgnoreCase(semester.getCourseCodes(), slot.getCourseCode())) {
            return SlotValidationResult.invalid("Course is not included in the semester anymore: " + slot.getCourseCode());
        }
        if (!containsIgnoreCase(semester.getRoomCodes(), slot.getRoomCode())) {
            return SlotValidationResult.invalid("Room is not included in the semester anymore: " + slot.getRoomCode());
        }

        Optional<Course> courseOpt = courseRepository.findByCodeIgnoreCase(slot.getCourseCode());
        if (courseOpt.isEmpty()) {
            return SlotValidationResult.invalid("Course not found: " + slot.getCourseCode());
        }
        Course course = courseOpt.get();
        if (course.getStatus() != Status.ACTIVE) {
            return SlotValidationResult.invalid("Course is not active: " + course.getCode());
        }

        Optional<Room> roomOpt = roomRepository.findByRoomCodeIgnoreCase(slot.getRoomCode());
        if (roomOpt.isEmpty()) {
            return SlotValidationResult.invalid("Room not found: " + slot.getRoomCode());
        }
        Room room = roomOpt.get();
        if (room.getStatus() != Status.ACTIVE) {
            return SlotValidationResult.invalid("Room is not active: " + room.getRoomCode());
        }
        if (room.getCapacity() < course.getRequiredRoomCapacity()) {
            return SlotValidationResult.invalid("Room capacity is too small for course: " + course.getCode());
        }
        if (!hasRequiredEquipment(room, course)) {
            return SlotValidationResult.invalid("Room equipment does not satisfy course requirements: " + course.getCode());
        }

        Optional<Teacher> teacherOpt = teacherRepository.findByTeacherId(course.getTeacherId());
        if (teacherOpt.isEmpty()) {
            return SlotValidationResult.invalid("Teacher not found: " + course.getTeacherId());
        }
        Teacher teacher = teacherOpt.get();
        if (teacher.getStatus() != Status.ACTIVE) {
            return SlotValidationResult.invalid("Teacher is not active: " + teacher.getTeacherId());
        }
        if (!isInsideTeacherWorkingHours(teacher, slot)) {
            return SlotValidationResult.invalid("Slot is outside teacher working hours: " + teacher.getTeacherId());
        }

        return SlotValidationResult.ok();
    }

    private boolean hasRequiredEquipment(Room room, Course course) {
        Map<String, Room.RoomItem> roomItemsByName = room.getItems().stream()
                .collect(Collectors.toMap(
                        item -> normalize(item.getName()),
                        Function.identity(),
                        (first, second) -> first
                ));

        for (Course.EquipmentRequirement requirement : course.getEquipmentRequirements()) {
            Room.RoomItem item = roomItemsByName.get(normalize(requirement.getName()));
            if (item == null || item.getQuantity() < requirement.getQuantity()) {
                return false;
            }
        }
        return true;
    }

    private boolean isInsideTeacherWorkingHours(Teacher teacher, ScheduleSlot slot) {
        for (TeacherWorkingHours wh : teacher.getPreferredWorkingHours()) {
            if (wh.getDay() == slot.getDayOfWeek()
                    && !wh.getStartTime().isAfter(slot.getStartTime())
                    && !wh.getEndTime().isBefore(slot.getEndTime())) {
                return true;
            }
        }
        return false;
    }

    private boolean containsIgnoreCase(List<String> values, String value) {
        if (values == null || value == null) {
            return false;
        }
        return values.stream().anyMatch(v -> v != null && v.equalsIgnoreCase(value));
    }

    private String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }
}
