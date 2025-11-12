package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.Status;
import ru.nsu.university.timetable.domain.User;
import ru.nsu.university.timetable.dto.CreateUserRequest;
import ru.nsu.university.timetable.dto.CreateUserResult;
import ru.nsu.university.timetable.dto.UpdateUserRequest;
import ru.nsu.university.timetable.dto.UserResponse;
import ru.nsu.university.timetable.repo.UserRepository;

import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class UserService {
    private final UserRepository repo;
    private final PasswordEncoder encoder;

    private static final String ABC = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnpqrstuvwxyz23456789!@#$%^&*";
    private String generatePassword(int len) {
        var rnd = new java.security.SecureRandom();
        var sb = new StringBuilder(len);
        for (int i = 0; i < len; i++) sb.append(ABC.charAt(rnd.nextInt(ABC.length())));
        return sb.toString();
    }

    public CreateUserResult create(CreateUserRequest req) {
        if (repo.existsByLogin(req.login())) throw new IllegalArgumentException("login already exists");
        if (repo.existsByEmail(req.email())) throw new IllegalArgumentException("email already exists");

        boolean generated = (req.password() == null || req.password().isBlank());
        String raw = generated ? generatePassword(16) : req.password();

        User u = User.builder()
                .login(req.login())
                .email(req.email())
                .passwordHash(encoder.encode(raw))
                .role(req.role())
                .status(Status.ACTIVE)
                .build();
        u = repo.save(u);

        return new CreateUserResult(map(u), generated ? raw : null);
    }

    @Transactional
    public UserResponse setPassword(UUID id, String rawPassword) {
        User u = repo.findById(id).orElseThrow();
        u.setPasswordHash(encoder.encode(rawPassword));
        return map(u);
    }

    public List<UserResponse> findAll() {
        return repo.findAll().stream().map(this::map).toList();
    }

    public UserResponse findOne(UUID id) {
        return repo.findById(id).map(this::map).orElseThrow();
    }

    public UserResponse update(UUID id, UpdateUserRequest req) {
        User u = repo.findById(id).orElseThrow();
        if (req.email() != null) u.setEmail(req.email());
        if (req.role() != null) u.setRole(req.role());
        if (req.status() != null) u.setStatus(req.status());
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

    public UserResponse map(User u) {
        return new UserResponse(
                u.getId(),
                u.getLogin(),
                u.getEmail(),
                u.getStatus(),
                u.getRole(),
                u.getCreatedAt(),
                u.getUpdatedAt()
        );
    }
}