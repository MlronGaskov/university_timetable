package ru.nsu.university.timetable.schedule;

import jakarta.persistence.*;

import java.time.LocalDate;
import java.util.UUID;

//TODO Implement use-cases and maybe change the path
//import ru.nsu.university.timetable.course.Course;
//import ru.nsu.university.timetable.teacher.Teacher;
//import ru.nsu.university.timetable.group.Group;
//import ru.nsu.university.timetable.group.Subgroup;
//import ru.nsu.university.timetable.room.Room;

@Entity
@Table(
        name = "class_sessions",
        indexes = {
                @Index(name = "idx_sessions_version", columnList = "version_id"),
                @Index(name = "idx_sessions_teacher_time", columnList = "teacher_id, date, slot_index"),
                @Index(name = "idx_sessions_group_time", columnList = "group_id, date, slot_index"),
                @Index(name = "idx_sessions_room_time", columnList = "room_id, date, slot_index")
        }
)
public class ClassSession {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "uuid")
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "version_id", nullable = false)
    private ScheduleVersion version;
//TODO Implement use-cases
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "course_id", nullable = false)
//    private Course course;
//
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "teacher_id", nullable = false)
//    private Teacher teacher;
//
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "group_id", nullable = false)
//    private Group group;
//
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "subgroup_id")
//    private Subgroup subgroup;
//
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "room_id", nullable = false)
//    private Room room;

    private LocalDate date;

    @Column(name = "slot_index")
    private Integer slotIndex;

    private Integer durationSlots;

    private boolean locked;

    public ClassSession() {
    }

    public UUID getId() {
        return id;
    }

    public ScheduleVersion getVersion() {
        return version;
    }

    //TODO Implement use-cases
//    public Course getCourse() {
//        return course;
//    }
//
//    public Teacher getTeacher() {
//        return teacher;
//    }
//
//    public Group getGroup() {
//        return group;
//    }
//
//    public Subgroup getSubgroup() {
//        return subgroup;
//    }
//
//    public Room getRoom() {
//        return room;
//    }

    public LocalDate getDate() {
        return date;
    }

    public Integer getSlotIndex() {
        return slotIndex;
    }

    public Integer getDurationSlots() {
        return durationSlots;
    }

    public boolean isLocked() {
        return locked;
    }

    public void setVersion(ScheduleVersion version) {
        this.version = version;
    }

    //TODO Implement use-cases
//    public void setCourse(Course course) {
//        this.course = course;
//    }
//
//    public void setTeacher(Teacher teacher) {
//        this.teacher = teacher;
//    }
//
//    public void setGroup(Group group) {
//        this.group = group;
//    }
//
//    public void setSubgroup(Subgroup subgroup) {
//        this.subgroup = subgroup;
//    }
//
//    public void setRoom(Room room) {
//        this.room = room;
//    }

    public void setDate(LocalDate date) {
        this.date = date;
    }

    public void setSlotIndex(Integer slotIndex) {
        this.slotIndex = slotIndex;
    }

    public void setDurationSlots(Integer durationSlots) {
        this.durationSlots = durationSlots;
    }

    public void setLocked(boolean locked) {
        this.locked = locked;
    }
}
