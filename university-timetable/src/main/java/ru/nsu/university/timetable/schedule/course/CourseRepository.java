package ru.nsu.university.timetable.schedule.course;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface CourseRepository extends JpaRepository<Course, UUID> {
    boolean existsByCodeIgnoreCase(String code);
}
