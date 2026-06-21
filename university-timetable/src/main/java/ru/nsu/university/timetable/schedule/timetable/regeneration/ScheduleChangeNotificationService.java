package ru.nsu.university.timetable.schedule.timetable.regeneration;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import ru.nsu.university.timetable.catalog.group.Group;
import ru.nsu.university.timetable.catalog.group.GroupRepository;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.schedule.course.CourseRepository;
import ru.nsu.university.timetable.schedule.semester.Semester;
import ru.nsu.university.timetable.schedule.timetable.Schedule;
import ru.nsu.university.timetable.schedule.timetable.ScheduleSlot;
import ru.nsu.university.timetable.user.auth.User;
import ru.nsu.university.timetable.user.auth.UserRepository;
import ru.nsu.university.timetable.user.student.Student;
import ru.nsu.university.timetable.user.student.StudentRepository;
import ru.nsu.university.timetable.user.teacher.Teacher;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

@Service
@Slf4j
@RequiredArgsConstructor
public class ScheduleChangeNotificationService {
    private static final String OUTPUT_FILE_NAME = "schedule-change-notifications.txt";

    private final CourseRepository courseRepository;
    private final GroupRepository groupRepository;
    private final StudentRepository studentRepository;
    private final TeacherRepository teacherRepository;
    private final UserRepository userRepository;

    public void notifyUsersAboutChanges(
            Semester semester,
            Schedule previousSchedule,
            Schedule nextSchedule,
            ScheduleChangeEvent event
    ) {
        try {
            Map<String, List<SlotSnapshot>> previousByCourse = slotsByCourse(previousSchedule);
            Map<String, List<SlotSnapshot>> nextByCourse = slotsByCourse(nextSchedule);
            Set<String> changedCourseCodes = changedCourseCodes(previousByCourse, nextByCourse);

            if (changedCourseCodes.isEmpty()) {
                log.info("No user notifications produced for semesterCode={} because no course-level changes were detected",
                        semester.getCode());
                return;
            }

            Map<String, RecipientNotification> notifications = new LinkedHashMap<>();
            for (String courseCode : changedCourseCodes) {
                Course course = courseRepository.findByCodeIgnoreCase(courseCode).orElse(null);
                List<String> changeLines = describeCourseChanges(
                        courseCode,
                        course,
                        previousByCourse.getOrDefault(courseCode, List.of()),
                        nextByCourse.getOrDefault(courseCode, List.of())
                );

                if (changeLines.isEmpty()) {
                    continue;
                }

                notifyTeacher(course, notifications, changeLines);
                notifyStudents(course, notifications, changeLines);
            }

            if (notifications.isEmpty()) {
                log.info("No matching recipients found for schedule change notifications, semesterCode={}, reason={}",
                        semester.getCode(), event.reason());
                return;
            }

            writeNotifications(semester, event, notifications.values());
        } catch (Exception ex) {
            log.warn("Failed to produce schedule change notifications for semesterCode={}",
                    semester.getCode(), ex);
        }
    }

    private Map<String, List<SlotSnapshot>> slotsByCourse(Schedule schedule) {
        Map<String, List<SlotSnapshot>> result = new LinkedHashMap<>();
        for (ScheduleSlot slot : schedule.getSlots()) {
            String courseCode = normalize(slot.getCourseCode());
            result.computeIfAbsent(courseCode, ignored -> new ArrayList<>())
                    .add(SlotSnapshot.from(slot));
        }

        for (List<SlotSnapshot> slots : result.values()) {
            slots.sort(Comparator.naturalOrder());
        }
        return result;
    }

    private Set<String> changedCourseCodes(
            Map<String, List<SlotSnapshot>> previousByCourse,
            Map<String, List<SlotSnapshot>> nextByCourse
    ) {
        Set<String> result = new LinkedHashSet<>();
        result.addAll(previousByCourse.keySet());
        result.addAll(nextByCourse.keySet());
        result.removeIf(courseCode -> Objects.equals(
                previousByCourse.getOrDefault(courseCode, List.of()),
                nextByCourse.getOrDefault(courseCode, List.of())
        ));
        return result;
    }

    private List<String> describeCourseChanges(
            String courseCode,
            Course course,
            List<SlotSnapshot> previousSlots,
            List<SlotSnapshot> nextSlots
    ) {
        Set<SlotSnapshot> previous = new LinkedHashSet<>(previousSlots);
        Set<SlotSnapshot> next = new LinkedHashSet<>(nextSlots);

        List<String> lines = new ArrayList<>();
        for (SlotSnapshot removed : previous) {
            if (!next.contains(removed)) {
                lines.add("removed " + formatCourseLabel(courseCode, course) + ": " + removed.toDisplayString());
            }
        }
        for (SlotSnapshot added : next) {
            if (!previous.contains(added)) {
                lines.add("added " + formatCourseLabel(courseCode, course) + ": " + added.toDisplayString());
            }
        }
        return lines;
    }

    private void notifyTeacher(
            Course course,
            Map<String, RecipientNotification> notifications,
            List<String> changeLines
    ) {
        if (course == null || course.getTeacherId() == null || course.getTeacherId().isBlank()) {
            return;
        }

        Teacher teacher = teacherRepository.findByTeacherId(course.getTeacherId()).orElse(null);
        User user = userRepository.findByTeacher_TeacherId(course.getTeacherId()).orElse(null);
        Recipient recipient = Recipient.forTeacher(course.getTeacherId(), teacher, user);
        notifications.computeIfAbsent(recipient.key(), ignored -> new RecipientNotification(recipient))
                .addChanges(changeLines);
    }

    private void notifyStudents(
            Course course,
            Map<String, RecipientNotification> notifications,
            List<String> changeLines
    ) {
        if (course == null || course.getGroupCodes() == null || course.getGroupCodes().isEmpty()) {
            return;
        }

        List<Group> groups = groupRepository.findByCodeIn(course.getGroupCodes());
        Set<String> studentIds = new LinkedHashSet<>();
        for (Group group : groups) {
            studentIds.addAll(safeList(group.getStudentIds()));
        }

        if (studentIds.isEmpty()) {
            return;
        }

        Map<String, Student> studentsById = new LinkedHashMap<>();
        for (Student student : studentRepository.findByStudentIdIn(studentIds)) {
            studentsById.put(student.getStudentId(), student);
        }

        Map<String, User> usersByStudentId = new LinkedHashMap<>();
        for (User user : userRepository.findByStudent_StudentIdIn(studentIds)) {
            if (user.getStudent() != null && user.getStudent().getStudentId() != null) {
                usersByStudentId.put(user.getStudent().getStudentId(), user);
            }
        }

        for (String studentId : studentIds) {
            Student student = studentsById.get(studentId);
            User user = usersByStudentId.get(studentId);
            Recipient recipient = Recipient.forStudent(studentId, student, user);
            notifications.computeIfAbsent(recipient.key(), ignored -> new RecipientNotification(recipient))
                    .addChanges(changeLines);
        }
    }

    private synchronized void writeNotifications(
            Semester semester,
            ScheduleChangeEvent event,
            Collection<RecipientNotification> notifications
    ) throws IOException {
        Path outputFile = resolveOutputFile();
        Path parent = outputFile.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }

        List<String> lines = new ArrayList<>();
        lines.add("==== " + Instant.now() + " ====");
        lines.add("semester=" + semester.getCode() + ", reason=" + event.reason());

        for (RecipientNotification notification : notifications) {
            lines.add("user=" + notification.recipient().display());
            for (String change : notification.changes()) {
                lines.add("  - " + change);
            }
        }
        lines.add("");

        Files.write(
                outputFile,
                lines,
                StandardCharsets.UTF_8,
                StandardOpenOption.CREATE,
                StandardOpenOption.APPEND
        );

        for (RecipientNotification notification : notifications) {
            log.info("Schedule change mail simulation for {}: {}",
                    notification.recipient().display(),
                    String.join(" | ", notification.changes()));
        }
        log.info("Schedule change notifications appended to {}", outputFile.toAbsolutePath());
    }

    private Path resolveOutputFile() {
        Path current = Paths.get("").toAbsolutePath();
        for (Path cursor = current; cursor != null; cursor = cursor.getParent()) {
            if (Files.isDirectory(cursor.resolve("scripts"))
                    && Files.isDirectory(cursor.resolve("university-timetable"))
                    && Files.isDirectory(cursor.resolve("university-timetable-web"))) {
                return cursor.resolve(OUTPUT_FILE_NAME);
            }
        }
        return current.resolve(OUTPUT_FILE_NAME);
    }

    private String formatCourseLabel(String courseCode, Course course) {
        if (course == null || course.getTitle() == null || course.getTitle().isBlank()) {
            return courseCode.toUpperCase(Locale.ROOT);
        }
        return course.getCode() + " (" + course.getTitle() + ")";
    }

    private List<String> safeList(List<String> values) {
        return values == null ? List.of() : values;
    }

    private String normalize(String value) {
        return value == null ? "" : value.trim().toLowerCase(Locale.ROOT);
    }

    private record SlotSnapshot(
            String roomCode,
            String dayOfWeek,
            String startTime,
            String endTime,
            String validFrom,
            String validUntil,
            String weekPattern
    ) implements Comparable<SlotSnapshot> {
        static SlotSnapshot from(ScheduleSlot slot) {
            return new SlotSnapshot(
                    slot.getRoomCode(),
                    String.valueOf(slot.getDayOfWeek()),
                    String.valueOf(slot.getStartTime()),
                    String.valueOf(slot.getEndTime()),
                    String.valueOf(slot.getValidFrom()),
                    String.valueOf(slot.getValidUntil()),
                    String.valueOf(slot.getWeekPattern())
            );
        }

        String toDisplayString() {
            return dayOfWeek + " " + startTime + "-" + endTime
                    + ", room=" + roomCode
                    + ", valid=" + validFrom + ".." + validUntil
                    + ", weeks=" + weekPattern;
        }

        @Override
        public int compareTo(SlotSnapshot other) {
            return Comparator.comparing(SlotSnapshot::dayOfWeek)
                    .thenComparing(SlotSnapshot::startTime)
                    .thenComparing(SlotSnapshot::endTime)
                    .thenComparing(SlotSnapshot::roomCode)
                    .thenComparing(SlotSnapshot::validFrom)
                    .thenComparing(SlotSnapshot::validUntil)
                    .thenComparing(SlotSnapshot::weekPattern)
                    .compare(this, other);
        }
    }

    private record Recipient(String key, String display) {
        static Recipient forTeacher(String teacherId, Teacher teacher, User user) {
            String displayName = teacher != null && teacher.getFullName() != null && !teacher.getFullName().isBlank()
                    ? teacher.getFullName()
                    : teacherId;
            String email = user != null ? user.getEmail() : "no-email";
            return new Recipient(
                    "teacher:" + teacherId,
                    "teacher " + displayName + " [" + teacherId + "] <" + email + ">"
            );
        }

        static Recipient forStudent(String studentId, Student student, User user) {
            String displayName = student != null && student.getFullName() != null && !student.getFullName().isBlank()
                    ? student.getFullName()
                    : studentId;
            String email = user != null ? user.getEmail() : "no-email";
            return new Recipient(
                    "student:" + studentId,
                    "student " + displayName + " [" + studentId + "] <" + email + ">"
            );
        }
    }

    private static final class RecipientNotification {
        private final Recipient recipient;
        private final Set<String> changes = new LinkedHashSet<>();

        private RecipientNotification(Recipient recipient) {
            this.recipient = recipient;
        }

        Recipient recipient() {
            return recipient;
        }

        List<String> changes() {
            return new ArrayList<>(changes);
        }

        void addChanges(Collection<String> changeLines) {
            changes.addAll(changeLines);
        }
    }
}
