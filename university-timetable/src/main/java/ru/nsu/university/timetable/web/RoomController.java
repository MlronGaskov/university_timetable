package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.rooms.CreateRoomRequest;
import ru.nsu.university.timetable.dto.rooms.RoomResponse;
import ru.nsu.university.timetable.dto.rooms.UpdateRoomRequest;
import ru.nsu.university.timetable.service.RoomService;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/rooms")
@RequiredArgsConstructor
public class RoomController {
    private final RoomService service;

    @GetMapping
    public List<RoomResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public RoomResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PostMapping
    @PreAuthorize("hasRole('ADMIN')")
    public RoomResponse create(@RequestBody @Validated CreateRoomRequest req) {
        return service.create(req);
    }

    @PutMapping("/{id}")
    @PreAuthorize("hasRole('ADMIN')")
    public RoomResponse update(@PathVariable UUID id,
                               @RequestBody @Validated UpdateRoomRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/archive")
    @PreAuthorize("hasRole('ADMIN')")
    public RoomResponse archive(@PathVariable UUID id) {
        return service.archive(id);
    }
}
