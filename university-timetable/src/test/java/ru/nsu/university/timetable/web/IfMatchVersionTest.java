package ru.nsu.university.timetable.web;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class IfMatchVersionTest {
    @Test
    void parseRequired_acceptsPlainNumber() {
        assertEquals(12L, IfMatchVersion.parseRequired("12"));
    }

    @Test
    void parseRequired_acceptsQuotedNumber() {
        assertEquals(12L, IfMatchVersion.parseRequired("\"12\""));
    }

    @Test
    void parseRequired_rejectsMissingHeader() {
        assertThrows(PreconditionRequiredException.class, () -> IfMatchVersion.parseRequired(null));
        assertThrows(PreconditionRequiredException.class, () -> IfMatchVersion.parseRequired(" "));
    }

    @Test
    void parseRequired_rejectsWildcardAndWeakEtags() {
        assertThrows(PreconditionRequiredException.class, () -> IfMatchVersion.parseRequired("*"));
        assertThrows(PreconditionRequiredException.class, () -> IfMatchVersion.parseRequired("W/\"1\""));
    }

    @Test
    void parseRequired_rejectsInvalidVersion() {
        assertThrows(PreconditionRequiredException.class, () -> IfMatchVersion.parseRequired("abc"));
        assertThrows(PreconditionRequiredException.class, () -> IfMatchVersion.parseRequired("-1"));
    }
}
