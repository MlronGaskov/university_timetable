package ru.nsu.university.timetable.user.auth;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface UserRepository extends JpaRepository<User, UUID> {
    Optional<User> findByLogin(String login);
    boolean existsByLogin(String login);
    boolean existsByEmail(String email);
    Optional<User> findByTeacher_Id(UUID teacherId);
    Optional<User> findByStudent_Id(UUID studentId);
}
