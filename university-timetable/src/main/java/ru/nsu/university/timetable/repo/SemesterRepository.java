package ru.nsu.university.timetable.repo;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import ru.nsu.university.timetable.domain.Semester;
import ru.nsu.university.timetable.domain.SemesterStatus;

public interface SemesterRepository extends JpaRepository<Semester, UUID> {

    boolean existsByCodeIgnoreCase(String code);

    Optional<Semester> findByCodeIgnoreCase(String code);

    @Query("""
            select s
            from Semester s
            where (:status is null or s.status = :status)
            order by s.start desc
            """)
    List<Semester> findByFilters(@Param("status") SemesterStatus status);
}

