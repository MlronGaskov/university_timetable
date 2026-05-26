package ru.nsu.university.timetable.schedule.timetable;

public enum ScheduleGenerationReason {
    FULL_GENERATION,
    COURSE_CHANGED,
    TEACHER_WORKING_HOURS_CHANGED,
    ROOM_CHANGED,
    SEMESTER_CHANGED,
    POLICY_CHANGED,
    GROUP_CHANGED,
    MANUAL_REGENERATION
}
