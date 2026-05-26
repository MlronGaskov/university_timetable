package ru.nsu.university.timetable.schedule.semester;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface SemesterRepository extends JpaRepository<Semester, UUID> {
    boolean existsByCodeIgnoreCase(String code);
    Optional<Semester> findByCodeIgnoreCase(String code);
    List<Semester> findByPolicyNameIgnoreCase(String policyName);

    @Query("select distinct s from Semester s join s.courseCodes c where lower(c) = lower(:courseCode)")
    List<Semester> findByCourseCodeIgnoreCase(@Param("courseCode") String courseCode);

    @Query("select distinct s from Semester s join s.roomCodes r where lower(r) = lower(:roomCode)")
    List<Semester> findByRoomCodeIgnoreCase(@Param("roomCode") String roomCode);

    @Query("""
            select distinct s
            from Semester s join s.courseCodes c, Course course
            where lower(c) = lower(course.code)
              and lower(course.teacherId) = lower(:teacherId)
            """)
    List<Semester> findByTeacherIdIgnoreCase(@Param("teacherId") String teacherId);

    @Query("""
            select distinct s
            from Semester s join s.courseCodes c, Course course join course.groupCodes g
            where lower(c) = lower(course.code)
              and lower(g) = lower(:groupCode)
            """)
    List<Semester> findByGroupCodeIgnoreCase(@Param("groupCode") String groupCode);
}
