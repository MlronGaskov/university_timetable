package ru.nsu.university.timetable.mapper;

import ru.nsu.university.timetable.schedule.ClassSession;
import ru.nsu.university.timetable.schedule.ScheduleVersion;
import ru.nsu.university.timetable.dto.ClassSessionDto;
import ru.nsu.university.timetable.dto.ScheduleVersionDto;

public final class ScheduleMapper {

    private ScheduleMapper() {
    }

    public static ScheduleVersionDto toDto(ScheduleVersion version) {
        if (version == null) {
            return null;
        }
        ScheduleVersionDto dto = new ScheduleVersionDto();
        dto.setId(version.getId());
        dto.setName(version.getName());

        //TODO implement this use-case
//        if (version.getSemester() != null) {
//            dto.setSemesterId(version.getSemester().getId());
//        }
        dto.setStatus(version.getStatus());
        dto.setPublished(version.isPublished());
        dto.setQualityMetrics(version.getQualityMetrics());
        dto.setCreatedAt(version.getCreatedAt());
        dto.setUpdatedAt(version.getUpdatedAt());
        return dto;
    }

    public static ClassSessionDto toDto(ClassSession session) {
        if (session == null) {
            return null;
        }
        ClassSessionDto dto = new ClassSessionDto();
        dto.setId(session.getId());
        if (session.getVersion() != null) {
            dto.setVersionId(session.getVersion().getId());
        }

        //TODO implement that use-cases
//        if (session.getCourse() != null) {
//            dto.setCourseId(session.getCourse().getId());
//        }
//        if (session.getTeacher() != null) {
//            dto.setTeacherId(session.getTeacher().getId());
//        }
//        if (session.getGroup() != null) {
//            dto.setGroupId(session.getGroup().getId());
//        }
//        if (session.getSubgroup() != null) {
//            dto.setSubgroupId(session.getSubgroup().getId());
//        }
//        if (session.getRoom() != null) {
//            dto.setRoomId(session.getRoom().getId());
//        }
        dto.setDate(session.getDate());
        dto.setSlotIndex(session.getSlotIndex());
        dto.setDurationSlots(session.getDurationSlots());
        dto.setLocked(session.isLocked());
        return dto;
    }
}
