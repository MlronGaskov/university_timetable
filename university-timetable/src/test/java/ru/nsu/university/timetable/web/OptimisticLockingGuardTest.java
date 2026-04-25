package ru.nsu.university.timetable.web;

import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

class OptimisticLockingGuardTest {
    @Test
    void verifyVersion_allowsMatchingVersions() {
        assertDoesNotThrow(() ->
                OptimisticLockingGuard.verifyVersion("Group", UUID.randomUUID(), 7L, 7L)
        );
    }

    @Test
    void verifyVersion_rejectsStaleVersion() {
        ResourceConflictException ex = assertThrows(
                ResourceConflictException.class,
                () -> OptimisticLockingGuard.verifyVersion("Group", UUID.randomUUID(), 7L, 8L)
        );

        assertTrue(ex.getMessage().contains("modified by another request"));
        assertTrue(ex.getMessage().contains("Expected version 7"));
        assertTrue(ex.getMessage().contains("actual version 8"));
    }
}
