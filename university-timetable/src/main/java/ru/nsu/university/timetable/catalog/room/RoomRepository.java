package ru.nsu.university.timetable.catalog.room;

import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;
import java.util.UUID;

public interface RoomRepository extends JpaRepository<Room, UUID> {
    boolean existsByBuildingIgnoreCaseAndNumberIgnoreCase(String building, String number);

    boolean existsByRoomCodeIgnoreCase(String roomCode);

    Optional<Room> findByRoomCodeIgnoreCase(String roomCode);
}
