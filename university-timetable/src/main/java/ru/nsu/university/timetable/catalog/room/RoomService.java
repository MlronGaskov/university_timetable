package ru.nsu.university.timetable.catalog.room;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.room.dto.CreateRoomRequest;
import ru.nsu.university.timetable.catalog.room.dto.RoomItemDto;
import ru.nsu.university.timetable.catalog.room.dto.RoomResponse;
import ru.nsu.university.timetable.catalog.room.dto.UpdateRoomRequest;

import java.util.Comparator;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class RoomService {
    private final RoomRepository roomRepository;

    public RoomResponse create(CreateRoomRequest req) {
        if (roomRepository.existsByBuildingIgnoreCaseAndNumberIgnoreCase(req.building(), req.number())) {
            throw new IllegalArgumentException(
                    "Room already exists in building '%s' with number '%s'"
                            .formatted(req.building(), req.number())
            );
        }

        Room room = Room.builder()
                .building(req.building())
                .number(req.number())
                .capacity(req.capacity())
                .status(Status.ACTIVE)
                .build();

        applyItems(room, req.items());

        return map(roomRepository.save(room));
    }

    @Transactional(readOnly = true)
    public List<RoomResponse> findAll() {
        return roomRepository.findAll().stream()
                .map(this::map)
                .toList();
    }

    @Transactional(readOnly = true)
    public RoomResponse findOne(UUID id) {
        return roomRepository.findById(id)
                .map(this::map)
                .orElseThrow(() -> new IllegalArgumentException("Room not found: " + id));
    }

    public RoomResponse update(UUID id, UpdateRoomRequest req) {
        Room room = roomRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Room not found: " + id));

        if (req.building() != null) {
            room.setBuilding(req.building());
        }

        if (req.number() != null) {
            room.setNumber(req.number());
        }

        if (req.capacity() != null) {
            room.setCapacity(req.capacity());
        }

        if (req.items() != null) {
            room.getItems().clear();
            applyItems(room, req.items());
        }

        return map(room);
    }

    public RoomResponse archive(UUID id) {
        Room room = roomRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Room not found: " + id));

        room.setStatus(Status.INACTIVE);
        return map(room);
    }

    public RoomResponse activate(UUID id) {
        Room room = roomRepository.findById(id)
                .orElseThrow(() -> new IllegalArgumentException("Room not found: " + id));

        room.setStatus(Status.ACTIVE);
        return map(room);
    }

    private void applyItems(Room room, List<RoomItemDto> dtos) {
        if (dtos == null || dtos.isEmpty()) {
            return;
        }
        for (RoomItemDto dto : dtos) {
            room.getItems().add(new Room.RoomItem(dto.name().trim().toLowerCase(), dto.quantity()));
        }
    }

    private RoomResponse map(Room r) {
        List<RoomItemDto> items = r.getItems().stream()
                .sorted(Comparator.comparing(Room.RoomItem::getName, String.CASE_INSENSITIVE_ORDER))
                .map(it -> new RoomItemDto(it.getName(), it.getQuantity()))
                .toList();

        return new RoomResponse(
                r.getId(),
                r.getBuilding(),
                r.getNumber(),
                r.getCapacity(),
                r.getStatus(),
                r.getCreatedAt(),
                r.getUpdatedAt(),
                items
        );
    }
}
