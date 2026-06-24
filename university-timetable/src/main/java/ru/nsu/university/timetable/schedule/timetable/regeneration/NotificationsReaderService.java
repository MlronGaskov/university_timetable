package ru.nsu.university.timetable.schedule.timetable.regeneration;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Service
@Slf4j
public class NotificationsReaderService {

    private static final Pattern HEADER_PATTERN = Pattern.compile("^==== (.+) ====$");
    private static final Pattern SEMESTER_PATTERN = Pattern.compile("^semester=([^,]+), reason=(.+)$");
    // user=teacher Попова Анна Ивановна [DCT0005] <t5@g.nsu.ru>
    private static final Pattern USER_PATTERN = Pattern.compile(
            "^user=(teacher|student) (.+) \\[([^]]+)] <([^>]+)>$"
    );
    private static final Pattern CHANGE_PATTERN = Pattern.compile("^  - (added|removed) (.+)$");

    public List<NotificationEntryDto> readAll() {
        Path file = resolveFile();
        if (!Files.exists(file)) {
            return List.of();
        }

        List<String> lines;
        try {
            lines = Files.readAllLines(file, StandardCharsets.UTF_8);
        } catch (IOException e) {
            log.warn("Failed to read notifications file: {}", file, e);
            return List.of();
        }

        return parse(lines);
    }

    private List<NotificationEntryDto> parse(List<String> lines) {
        List<NotificationEntryDto> entries = new ArrayList<>();

        String timestamp = null;
        String semesterCode = null;
        String reason = null;
        List<NotificationEntryDto.UserNotificationDto> users = new ArrayList<>();

        // current user state
        String userType = null;
        String fullName = null;
        String userId = null;
        String email = null;
        List<NotificationEntryDto.SlotChangeDto> changes = new ArrayList<>();

        for (String line : lines) {
            Matcher headerMatcher = HEADER_PATTERN.matcher(line);
            if (headerMatcher.matches()) {
                // save previous entry if exists
                if (timestamp != null) {
                    flushUser(users, userType, fullName, userId, email, changes);
                    entries.add(new NotificationEntryDto(timestamp, semesterCode, reason, new ArrayList<>(users)));
                }
                timestamp = headerMatcher.group(1);
                semesterCode = null;
                reason = null;
                users = new ArrayList<>();
                userType = null;
                fullName = null;
                userId = null;
                email = null;
                changes = new ArrayList<>();
                continue;
            }

            Matcher semMatcher = SEMESTER_PATTERN.matcher(line);
            if (semMatcher.matches()) {
                semesterCode = semMatcher.group(1).trim();
                reason = semMatcher.group(2).trim();
                continue;
            }

            Matcher userMatcher = USER_PATTERN.matcher(line);
            if (userMatcher.matches()) {
                // flush previous user
                flushUser(users, userType, fullName, userId, email, changes);
                userType = userMatcher.group(1);
                fullName = userMatcher.group(2).trim();
                userId = userMatcher.group(3).trim();
                email = userMatcher.group(4).trim();
                changes = new ArrayList<>();
                continue;
            }

            Matcher changeMatcher = CHANGE_PATTERN.matcher(line);
            if (changeMatcher.matches()) {
                String action = changeMatcher.group(1);
                String desc = changeMatcher.group(2).trim();
                changes.add(new NotificationEntryDto.SlotChangeDto(action, desc));
            }
        }

        // flush last entry
        if (timestamp != null) {
            flushUser(users, userType, fullName, userId, email, changes);
            entries.add(new NotificationEntryDto(timestamp, semesterCode, reason, new ArrayList<>(users)));
        }

        // newest first
        entries.sort((a, b) -> b.timestamp().compareTo(a.timestamp()));
        return entries;
    }

    private void flushUser(
            List<NotificationEntryDto.UserNotificationDto> users,
            String userType, String fullName, String userId, String email,
            List<NotificationEntryDto.SlotChangeDto> changes
    ) {
        if (userType != null && userId != null) {
            users.add(new NotificationEntryDto.UserNotificationDto(
                    userType, fullName, userId, email, new ArrayList<>(changes)
            ));
        }
    }

    private Path resolveFile() {
        Path current = Paths.get("").toAbsolutePath();
        for (Path cursor = current; cursor != null; cursor = cursor.getParent()) {
            if (Files.isDirectory(cursor.resolve("scripts"))
                    && Files.isDirectory(cursor.resolve("university-timetable"))
                    && Files.isDirectory(cursor.resolve("university-timetable-web"))) {
                return cursor.resolve("schedule-change-notifications.txt");
            }
        }
        // in Docker, file is written to /app/schedule-change-notifications.txt
        Path docker = Paths.get("/app/schedule-change-notifications.txt");
        if (Files.exists(docker)) {
            return docker;
        }
        return current.resolve("schedule-change-notifications.txt");
    }
}
