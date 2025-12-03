package ru.nsu.university.timetable.user.teacher;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface TeacherRepository extends JpaRepository<Teacher, UUID> {
    boolean existsByFullNameIgnoreCase(String fullName);
}
