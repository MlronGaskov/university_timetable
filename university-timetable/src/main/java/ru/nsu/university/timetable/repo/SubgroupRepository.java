package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.Subgroup;

import java.util.UUID;

public interface SubgroupRepository extends JpaRepository<Subgroup, UUID> {
    boolean existsByGroupIdAndNameIgnoreCase(UUID groupId, String name);
}
