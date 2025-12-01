package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.RoomEquipment;

import java.util.UUID;

public interface RoomEquipmentRepository extends JpaRepository<RoomEquipment, UUID> {
}
