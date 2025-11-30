package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.Group;

import java.util.UUID;
import java.util.Optional;

public interface GroupRepository extends JpaRepository<Group, UUID> {
    boolean existsByCode(String code);

    Optional<Group> findByCode(String code);
}
