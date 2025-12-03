package ru.nsu.university.timetable.catalog.policy;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface PolicyRepository extends JpaRepository<Policy, UUID> {
    boolean existsByNameIgnoreCase(String name);

    Optional<Policy> findByNameIgnoreCase(String name);
}
