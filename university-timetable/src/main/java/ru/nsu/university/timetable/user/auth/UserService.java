package ru.nsu.university.timetable.user.auth;

import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.user.auth.dto.CreateUserRequest;
import ru.nsu.university.timetable.user.auth.dto.CreateUserResult;
import ru.nsu.university.timetable.user.auth.dto.UpdateUserRequest;
import ru.nsu.university.timetable.user.auth.dto.UserResponse;
import ru.nsu.university.timetable.user.student.Student;
import ru.nsu.university.timetable.user.student.StudentRepository;
import ru.nsu.university.timetable.user.teacher.Teacher;
import ru.nsu.university.timetable.user.teacher.TeacherRepository;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class UserService {
    private static final String ABC = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz23456789!@#$%^&*";
    private final UserRepository repo;
    private final TeacherRepository teacherRepository;
    private final StudentRepository studentRepository;
    private final PasswordEncoder encoder;

    private String generatePassword(int len) {
        var rnd = new java.security.SecureRandom();
        var sb = new StringBuilder(len);
        for (int i = 0; i < len; i++) {
            sb.append(ABC.charAt(rnd.nextInt(ABC.length())));
        }
        return sb.toString();
    }

    public CreateUserResult create(CreateUserRequest req) {
        if (repo.existsByLogin(req.login())) {
            throw new IllegalArgumentException("login already exists");
        }
        if (repo.existsByEmail(req.email())) {
            throw new IllegalArgumentException("email already exists");
        }

        boolean generated = (req.password() == null || req.password().isBlank());
        String raw = generated ? generatePassword(16) : req.password();

        User user = new User();
        user.setLogin(req.login());
        user.setEmail(req.email());
        user.setPasswordHash(encoder.encode(raw));
        user.setStatus(Status.ACTIVE);

        applyRoleBindings(user, req.role(), req.teacherId(), req.studentId());

        user = repo.save(user);

        return new CreateUserResult(map(user), generated ? raw : null);
    }

    @Transactional
    public UserResponse setPassword(UUID id, String rawPassword) {
        User u = repo.findById(id).orElseThrow();
        u.setPasswordHash(encoder.encode(rawPassword));
        return map(u);
    }

    @Transactional(readOnly = true)
    public List<UserResponse> findAll() {
        return repo.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public UserResponse findOne(UUID id) {
        return repo.findById(id)
                .map(this::map)
                .orElseThrow();
    }

    public UserResponse update(UUID id, UpdateUserRequest req) {
        User u = repo.findById(id).orElseThrow();

        if (req.email() != null) {
            u.setEmail(req.email());
        }

        Role targetRole = req.role() != null ? req.role() : u.getRole();

        String teacherId = req.teacherId() != null
                ? req.teacherId()
                : (u.getTeacher() != null ? u.getTeacher().getTeacherId() : null);

        String studentId = req.studentId() != null
                ? req.studentId()
                : (u.getStudent() != null ? u.getStudent().getStudentId() : null);

        applyRoleBindings(u, targetRole, teacherId, studentId);

        if (req.status() != null) {
            u.setStatus(req.status());
        }

        return map(u);
    }

    public UserResponse deactivate(UUID id) {
        User u = repo.findById(id).orElseThrow();
        u.setStatus(Status.INACTIVE);
        return map(u);
    }

    public UserResponse activate(UUID id) {
        User u = repo.findById(id).orElseThrow();
        u.setStatus(Status.ACTIVE);
        return map(u);
    }

    private void applyRoleBindings(User user, Role role, String teacherId, String studentId) {
        if (role == null) {
            throw new IllegalArgumentException("role must not be null");
        }

        switch (role) {
            case ADMIN -> {
                if (teacherId != null || studentId != null) {
                    throw new IllegalArgumentException("ADMIN cannot be linked to teacher or student");
                }
                user.setRole(Role.ADMIN);
                user.setTeacher(null);
                user.setStudent(null);
            }
            case TEACHER -> configureTeacherRole(user, teacherId);
            case STUDENT -> configureStudentRole(user, studentId);
        }
    }

    private void configureTeacherRole(User user, String teacherId) {
        if (teacherId == null || teacherId.isBlank()) {
            throw new IllegalArgumentException("teacherId is required for TEACHER role");
        }
        String normalized = teacherId.trim();

        repo.findByTeacher_TeacherId(normalized)
                .filter(existing -> !existing.getId().equals(user.getId()))
                .ifPresent(existing -> {
                    throw new IllegalArgumentException("Teacher is already linked to another user");
                });

        Teacher teacher = teacherRepository.findByTeacherId(normalized)
                .orElseThrow(() -> new IllegalArgumentException("Teacher not found: " + normalized));

        user.setRole(Role.TEACHER);
        user.setTeacher(teacher);
        user.setStudent(null);
    }

    private void configureStudentRole(User user, String studentId) {
        if (studentId == null || studentId.isBlank()) {
            throw new IllegalArgumentException("studentId is required for STUDENT role");
        }
        String normalized = studentId.trim();

        repo.findByStudent_StudentId(normalized)
                .filter(existing -> !existing.getId().equals(user.getId()))
                .ifPresent(existing -> {
                    throw new IllegalArgumentException("Student is already linked to another user");
                });

        Student student = studentRepository.findByStudentId(normalized)
                .orElseThrow(() -> new IllegalArgumentException("Student not found: " + normalized));

        user.setRole(Role.STUDENT);
        user.setStudent(student);
        user.setTeacher(null);
    }

    private UserResponse map(User u) {
        String teacherId = null;
        String teacherName = null;
        if (u.getTeacher() != null) {
            teacherId = u.getTeacher().getTeacherId();
            teacherName = u.getTeacher().getFullName();
        }

        String studentId = null;
        String studentName = null;
        if (u.getStudent() != null) {
            studentId = u.getStudent().getStudentId();
            studentName = u.getStudent().getFullName();
        }

        return new UserResponse(
                u.getId(),
                u.getLogin(),
                u.getEmail(),
                u.getStatus(),
                u.getRole(),
                teacherId,
                teacherName,
                studentId,
                studentName,
                u.getCreatedAt(),
                u.getUpdatedAt()
        );
    }
}
