package ru.nsu.university.timetable.schedule.timetable.regeneration;

public record SlotValidationResult(boolean valid, String reason) {
    public static SlotValidationResult ok() {
        return new SlotValidationResult(true, null);
    }

    public static SlotValidationResult invalid(String reason) {
        return new SlotValidationResult(false, reason);
    }
}
