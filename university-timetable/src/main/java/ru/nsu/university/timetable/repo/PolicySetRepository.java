package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.PolicySet;
import ru.nsu.university.timetable.domain.PolicySetStatus;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

public interface PolicySetRepository extends JpaRepository<PolicySet, UUID> {

    Optional<PolicySet> findFirstByStatusOrderByCreatedAtDesc(PolicySetStatus status);

    Optional<PolicySet> findFirstByOrderByVersionDesc();

    List<PolicySet> findAllByOrderByCreatedAtDesc();
}
