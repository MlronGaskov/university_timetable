package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import ru.nsu.university.timetable.schedule.ClassSession;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

public interface ClassSessionRepository extends JpaRepository<ClassSession, UUID> {

    List<ClassSession> findByVersionId(UUID versionId);

    List<ClassSession> findByVersionIdAndGroupId(UUID versionId, UUID groupId);

    List<ClassSession> findByVersionIdAndTeacherId(UUID versionId, UUID teacherId);

    List<ClassSession> findByVersionIdAndRoomId(UUID versionId, UUID roomId);

    @Query("""
            select s from ClassSession s
            where s.version.id = :versionId
              and s.id <> :sessionId
              and s.date = :date
              and s.slotIndex = :slotIndex
              and (s.teacher.id = :teacherId
                   or s.group.id    = :groupId
                   or s.room.id     = :roomId)
            """)
    List<ClassSession> findConflicts(
            @Param("versionId") UUID versionId,
            @Param("sessionId") UUID sessionId,
            @Param("date") LocalDate date,
            @Param("slotIndex") Integer slotIndex,
            @Param("teacherId") UUID teacherId,
            @Param("groupId") UUID groupId,
            @Param("roomId") UUID roomId
    );
}
