package ru.nsu.university.timetable.schedule.semester;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface SemesterRepository extends JpaRepository<Semester, UUID> {
    boolean existsByCodeIgnoreCase(String code);
    Optional<Semester> findByCodeIgnoreCase(String code);
}
