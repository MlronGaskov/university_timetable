package ru.nsu.university.timetable.repo;

import org.springframework.data.jpa.repository.JpaRepository;
import ru.nsu.university.timetable.domain.Room;

import java.util.UUID;

public interface RoomRepository extends JpaRepository<Room, UUID> {
    boolean existsByBuildingAndNumber(String building, String number);
}
