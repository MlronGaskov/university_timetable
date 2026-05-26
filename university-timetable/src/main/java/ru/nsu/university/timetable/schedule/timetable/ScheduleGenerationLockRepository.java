package ru.nsu.university.timetable.schedule.timetable;

import jakarta.persistence.LockModeType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface ScheduleGenerationLockRepository extends JpaRepository<ScheduleGenerationLock, String> {
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select l from ScheduleGenerationLock l where l.semesterCode = :semesterCode")
    Optional<ScheduleGenerationLock> findForUpdate(@Param("semesterCode") String semesterCode);
}
