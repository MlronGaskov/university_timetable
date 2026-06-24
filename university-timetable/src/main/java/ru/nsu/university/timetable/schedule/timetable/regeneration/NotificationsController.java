package ru.nsu.university.timetable.schedule.timetable.regeneration;

import lombok.RequiredArgsConstructor;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

@RestController
@RequestMapping("/api/notifications")
@RequiredArgsConstructor
public class NotificationsController {

    private final NotificationsReaderService notificationsReaderService;

    @GetMapping
    @PreAuthorize("hasRole('ADMIN')")
    public List<NotificationEntryDto> getAll() {
        return notificationsReaderService.readAll();
    }
}
