package ru.nsu.university.timetable.web;

import java.util.UUID;

public final class OptimisticLockingGuard {
    private OptimisticLockingGuard() {
    }

    public static void verifyVersion(String resourceName, UUID resourceId, long expectedVersion, long actualVersion) {
        if (expectedVersion != actualVersion) {
            throw new ResourceConflictException(
                    "%s %s was modified by another request. Expected version %d, actual version %d. Please reload and retry."
                            .formatted(resourceName, resourceId, expectedVersion, actualVersion)
            );
        }
    }
}
