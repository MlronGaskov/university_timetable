package ru.nsu.university.timetable.schedule.timetable.regeneration;

import java.util.List;

public record NotificationEntryDto(
        String timestamp,
        String semesterCode,
        String reason,
        List<UserNotificationDto> users
) {
    public record UserNotificationDto(
            String userType,
            String fullName,
            String userId,
            String email,
            List<SlotChangeDto> changes
    ) {}

    public record SlotChangeDto(
            String action,
            String description
    ) {}
}
