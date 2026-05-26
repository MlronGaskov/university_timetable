package ru.nsu.university.timetable.schedule.timetable.regeneration;

import ru.nsu.university.timetable.schedule.timetable.ScheduleGenerationReason;

public record ScheduleChangeEvent(
        ScheduleGenerationReason reason,
        String courseCode,
        String teacherId,
        String roomCode,
        String semesterCode,
        String policyName,
        String groupCode
) {
    public static ScheduleChangeEvent courseChanged(String courseCode) {
        return new ScheduleChangeEvent(ScheduleGenerationReason.COURSE_CHANGED, courseCode, null, null, null, null, null);
    }

    public static ScheduleChangeEvent teacherWorkingHoursChanged(String teacherId) {
        return new ScheduleChangeEvent(ScheduleGenerationReason.TEACHER_WORKING_HOURS_CHANGED, null, teacherId, null, null, null, null);
    }

    public static ScheduleChangeEvent roomChanged(String roomCode) {
        return new ScheduleChangeEvent(ScheduleGenerationReason.ROOM_CHANGED, null, null, roomCode, null, null, null);
    }

    public static ScheduleChangeEvent semesterChanged(String semesterCode) {
        return new ScheduleChangeEvent(ScheduleGenerationReason.SEMESTER_CHANGED, null, null, null, semesterCode, null, null);
    }

    public static ScheduleChangeEvent policyChanged(String policyName) {
        return new ScheduleChangeEvent(ScheduleGenerationReason.POLICY_CHANGED, null, null, null, null, policyName, null);
    }

    public static ScheduleChangeEvent groupChanged(String groupCode) {
        return new ScheduleChangeEvent(ScheduleGenerationReason.GROUP_CHANGED, null, null, null, null, null, groupCode);
    }
}
