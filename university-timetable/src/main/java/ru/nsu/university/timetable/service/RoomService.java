package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import ru.nsu.university.timetable.domain.*;
import ru.nsu.university.timetable.dto.rooms.*;
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
        if (req.number() != null)  r.setNumber(req.number());
        if (req.capacity() != null) r.setCapacity(req.capacity());

        if (req.equipment() != null) {
            r.getEquipment().clear();
            applyEquipment(r, req.equipment());
        }

        return map(r);
    }

    public RoomResponse archive(UUID id) {
        Room r = repo.findById(id).orElseThrow();
        r.setStatus(Status.INACTIVE);
        return map(r);
    }

    private void applyEquipment(Room r, List<RoomEquipmentItem> items) {
        if (items == null) return;
        List<RoomEquipment> list = new ArrayList<>();
        for (RoomEquipmentItem item : items) {
            RoomEquipment eq = RoomEquipment.builder()
                    .room(r)
                    .name(item.name())
                    .quantity(item.quantity())
                    .build();
            list.add(eq);
        }
        r.getEquipment().addAll(list);
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
