package ru.nsu.university.timetable.service;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.web.server.ResponseStatusException;
import ru.nsu.university.timetable.schedule.ScheduleLock;
import ru.nsu.university.timetable.repo.ScheduleLockRepository;

import java.time.Duration;
import java.time.Instant;
import java.util.Optional;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class ScheduleLockService {

    private final ScheduleLockRepository repository;

    public void acquire(UUID versionId) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Not authenticated");
        }
        String username = auth.getName();

        Optional<ScheduleLock> existing = repository.findById(versionId);
        Instant now = Instant.now();

        if (existing.isPresent() && existing.get().getExpiresAt().isAfter(now)
                && !existing.get().getOwnerUsername().equals(username)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "Version locked by another user");
        }

        ScheduleLock lock = existing.orElseGet(ScheduleLock::new);
        lock.setVersionId(versionId);
        lock.setOwnerUsername(username);
        lock.setAcquiredAt(now);
        lock.setExpiresAt(now.plus(Duration.ofMinutes(30)));

        repository.save(lock);
    }

    public void ensureHasLock(UUID versionId) {
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        if (auth == null || !auth.isAuthenticated()) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Not authenticated");
        }
        String username = auth.getName();

        ScheduleLock lock = repository.findById(versionId)
                .orElseThrow(() -> new ResponseStatusException(HttpStatus.CONFLICT, "No lock for version"));

        if (!lock.getOwnerUsername().equals(username)) {
            throw new ResponseStatusException(HttpStatus.CONFLICT, "Lock held by another user");
        }
    }

    public void release(UUID versionId) {
        repository.deleteById(versionId);
    }
}
