package ru.nsu.university.timetable.domain;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.Instant;
import java.util.EnumSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(
        name = "courses",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_courses_code",
                columnNames = "code"
        )
)
@EntityListeners(AuditingEntityListener.class)
public class Course {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, length = 64)
    private String code;

    @Column(nullable = false, length = 256)
    private String title;

    @Column(name = "department", length = 128)
    private String department;

    @ElementCollection(fetch = FetchType.EAGER, targetClass = ActivityForm.class)
    @CollectionTable(name = "course_activity_forms", joinColumns = @JoinColumn(name = "course_id"))
    @Enumerated(EnumType.STRING)
    @Column(name = "activity_form", length = 16, nullable = false)
    private Set<ActivityForm> activityForms = EnumSet.noneOf(ActivityForm.class);

    @Column(name = "lecture_hours")
    private int lectureHours;

    @Column(name = "seminar_hours")
    private int seminarHours;

    @Column(name = "lab_hours")
    private int labHours;

    @Column(name = "room_requirements", length = 512)
    private String roomRequirements;

    @ManyToMany
    @JoinTable(
            name = "course_equipment_requirements",
            joinColumns = @JoinColumn(name = "course_id"),
            inverseJoinColumns = @JoinColumn(name = "equipment_id")
    )
    private Set<RoomEquipment> equipmentRequirements;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private CourseStatus status;

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;

    public int getTotalPlannedHours() {
        return lectureHours + seminarHours + labHours;
    }
}
