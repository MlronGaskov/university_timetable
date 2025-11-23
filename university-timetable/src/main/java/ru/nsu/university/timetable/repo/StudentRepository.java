package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.Student;

import java.util.UUID;

public interface StudentRepository extends JpaRepository<Student, UUID> {
    boolean existsByStudentId(String studentId);
}
