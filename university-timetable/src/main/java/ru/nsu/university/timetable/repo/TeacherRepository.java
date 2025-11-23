package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.Teacher;

import java.util.UUID;

public interface TeacherRepository extends JpaRepository<Teacher, UUID> {
    boolean existsByFullNameIgnoreCase(String fullName);
}
