package ru.nsu.university.timetable.user.auth;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.user.auth.dto.*;
import ru.nsu.university.timetable.web.IfMatchVersion;

import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/users")
@RequiredArgsConstructor
@PreAuthorize("hasRole('ADMIN')")
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
    public UserResponse update(@PathVariable UUID id,
                               @RequestHeader(name = "If-Match", required = false) String ifMatch,
                               @RequestBody @Validated UpdateUserRequest req) {
        return service.update(id, IfMatchVersion.parseRequired(ifMatch), req);
    }

    @PostMapping("/{id}/password")
    public UserResponse setPassword(@PathVariable UUID id,
                                    @RequestHeader(name = "If-Match", required = false) String ifMatch,
                                    @RequestBody @Validated SetPasswordRequest req) {
        return service.setPassword(id, IfMatchVersion.parseRequired(ifMatch), req.newPassword());
    }

    @PostMapping("/{id}/deactivate")
    public UserResponse deactivate(@PathVariable UUID id,
                                   @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.deactivate(id, IfMatchVersion.parseRequired(ifMatch));
    }

    @PostMapping("/{id}/activate")
    public UserResponse activate(@PathVariable UUID id,
                                 @RequestHeader(name = "If-Match", required = false) String ifMatch) {
        return service.activate(id, IfMatchVersion.parseRequired(ifMatch));
    }
}
