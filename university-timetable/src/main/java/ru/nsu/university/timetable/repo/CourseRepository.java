package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import ru.nsu.university.timetable.domain.ActivityForm;
import ru.nsu.university.timetable.domain.Course;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface CourseRepository extends JpaRepository<Course, UUID> {

    boolean existsByCodeIgnoreCase(String code);

    Optional<Course> findByCodeIgnoreCase(String code);

    @Query("""
            select c
            from Course c
            where (:department is null or c.department = :department)
              and (:form is null or :form member of c.activityForms)
            """)
    List<Course> findByFilters(@Param("department") String department,
                               @Param("form") ActivityForm form);
}
