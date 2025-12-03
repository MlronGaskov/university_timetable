package ru.nsu.university.timetable.user.teacher;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface TeacherRepository extends JpaRepository<Teacher, UUID> {
    boolean existsByFullNameIgnoreCase(String fullName);

    boolean existsByTeacherId(String teacherId);

    Optional<Teacher> findByTeacherId(String teacherId);
}
