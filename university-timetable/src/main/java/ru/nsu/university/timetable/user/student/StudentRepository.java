package ru.nsu.university.timetable.user.student;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface StudentRepository extends JpaRepository<Student, UUID> {
    boolean existsByStudentId(String studentId);

    Optional<Student> findByStudentId(String studentId);
}
