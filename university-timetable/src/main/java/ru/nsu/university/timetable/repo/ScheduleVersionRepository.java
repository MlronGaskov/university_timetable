package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import ru.nsu.university.timetable.schedule.ScheduleVersion;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface ScheduleVersionRepository extends JpaRepository<ScheduleVersion, UUID> {

    List<ScheduleVersion> findBySemesterIdOrderByCreatedAtDesc(UUID semesterId);

    @Query("""
        select v
        from ScheduleVersion v
        where v.semester.id = :semesterId and v.status = ru.nsu.university.timetable.schedule.ScheduleVersionStatus.ACTIVE
        """)
    Optional<ScheduleVersion> findActiveBySemesterId(@Param("semesterId") UUID semesterId);
}
