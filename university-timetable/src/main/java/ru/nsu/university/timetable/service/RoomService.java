package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.Room;
import ru.nsu.university.timetable.domain.RoomEquipment;
import ru.nsu.university.timetable.domain.Status;
import ru.nsu.university.timetable.dto.rooms.CreateRoomRequest;
import ru.nsu.university.timetable.dto.rooms.RoomEquipmentDto;
import ru.nsu.university.timetable.dto.rooms.RoomEquipmentItem;
import ru.nsu.university.timetable.dto.rooms.RoomResponse;
import ru.nsu.university.timetable.dto.rooms.UpdateRoomRequest;
import ru.nsu.university.timetable.repo.RoomRepository;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Transactional
public class RoomService {
    private final RoomRepository repo;

    public RoomResponse create(CreateRoomRequest req) {
        if (repo.existsByBuildingAndNumber(req.building(), req.number()))
            throw new IllegalArgumentException("room already exists in building");

        Room r = Room.builder()
                .building(req.building())
                .number(req.number())
                .capacity(req.capacity())
                .status(Status.ACTIVE)
                .build();

        applyEquipment(r, req.equipment());
        return map(repo.save(r));
    }

    public List<RoomResponse> findAll() {
        return repo.findAll().stream().map(this::map).toList();
    }

    public RoomResponse findOne(UUID id) {
        return repo.findById(id).map(this::map).orElseThrow();
    }

    public RoomResponse update(UUID id, UpdateRoomRequest req) {
        Room r = repo.findById(id).orElseThrow();

        if (req.building() != null) r.setBuilding(req.building());
        if (req.number() != null) r.setNumber(req.number());
        if (req.capacity() != null) r.setCapacity(req.capacity());

        if (req.equipment() != null) {
            applyEquipmentDiff(r, req.equipment());
        }
        repo.save(r);

        return map(r);
    }

    public RoomResponse archive(UUID id) {
        Room r = repo.findById(id).orElseThrow();
        r.setStatus(Status.INACTIVE);
        return map(r);
    }

    private void applyEquipment(Room r, List<RoomEquipmentItem> items) {
        if (items == null) return;
        for (RoomEquipmentItem item : items) {
            RoomEquipment eq = RoomEquipment.builder()
                    .room(r)
                    .name(item.name())
                    .quantity(item.quantity())
                    .build();
            r.getEquipment().add(eq);
        }
    }

    private void applyEquipmentDiff(Room r, List<RoomEquipmentItem> items) {
        var itemsByName = items.stream()
                .collect(java.util.stream.Collectors.toMap(
                        RoomEquipmentItem::name,
                        i -> i,
                        (a, b) -> b
                ));

        r.getEquipment().removeIf(eq -> {
            RoomEquipmentItem item = itemsByName.get(eq.getName());
            return item == null || item.quantity() <= 0;
        });

        for (RoomEquipmentItem item : items) {
            if (item.quantity() == 0) {
                continue;
            }

            var existingByName = r.getEquipment().stream()
                    .collect(java.util.stream.Collectors.toMap(
                            RoomEquipment::getName,
                            e -> e
                    ));
            RoomEquipment existing = existingByName.get(item.name());
            if (existing != null) {
                existing.setQuantity(item.quantity());
            } else {
                RoomEquipment eq = RoomEquipment.builder()
                        .room(r)
                        .name(item.name())
                        .quantity(item.quantity())
                        .build();
                r.getEquipment().add(eq);
            }
        }
    }

    private RoomResponse map(Room r) {
        List<RoomEquipmentDto> eq = r.getEquipment().stream()
                .map(e -> new RoomEquipmentDto(e.getId(), e.getName(), e.getQuantity()))
                .toList();

        return new RoomResponse(
                r.getId(),
                r.getBuilding(),
                r.getNumber(),
                r.getCapacity(),
                r.getStatus(),
                r.getCreatedAt(),
                r.getUpdatedAt(),
                eq
        );
    }
}
