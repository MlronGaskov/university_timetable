package ru.nsu.university.timetable.schedule.timetable;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ScheduleRepository extends JpaRepository<Schedule, UUID> {
    List<Schedule> findBySemester_CodeOrderByVersionDesc(String semesterCode);
    Optional<Schedule> findTopBySemester_CodeOrderByVersionDesc(String semesterCode);
}
