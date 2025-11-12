package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.*;
import ru.nsu.university.timetable.service.UserService;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
public class UserController {
    private final UserService service;

    @PostMapping
    public CreateUserResult create(@RequestBody @Validated CreateUserRequest req) {
        return service.create(req);
    }

    @GetMapping
    public List<UserResponse> all() {
        return service.findAll();
    }

    @GetMapping("/{id}")
    public UserResponse one(@PathVariable UUID id) {
        return service.findOne(id);
    }

    @PutMapping("/{id}")
    public UserResponse update(@PathVariable UUID id, @RequestBody @Validated UpdateUserRequest req) {
        return service.update(id, req);
    }

    @PostMapping("/{id}/password")
    public UserResponse setPassword(@PathVariable UUID id, @RequestBody @Validated SetPasswordRequest req) {
        return service.setPassword(id, req.newPassword());
    }

    @PostMapping("/{id}/deactivate")
    public UserResponse deactivate(@PathVariable UUID id) {
        return service.deactivate(id);
    }

    @PostMapping("/{id}/activate")
    public UserResponse activate(@PathVariable UUID id) {
        return service.activate(id);
    }
}