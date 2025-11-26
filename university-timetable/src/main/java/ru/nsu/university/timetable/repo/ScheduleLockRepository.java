package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.schedule.ScheduleLock;

import java.util.UUID;

public interface ScheduleLockRepository extends JpaRepository<ScheduleLock, UUID> {
}
