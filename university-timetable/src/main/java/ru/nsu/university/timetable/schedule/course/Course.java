package ru.nsu.university.timetable.schedule.course;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
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
                columnNames = {"code"}
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

    @Column(name = "teacher_id", nullable = false, length = 64)
    private String teacherId;

    @ElementCollection
    @CollectionTable(
            name = "course_groups",
            joinColumns = @JoinColumn(name = "course_id")
    )
    @Column(name = "group_code", nullable = false, length = 64)
    @Builder.Default
    private List<String> groupCodes = new ArrayList<>();

    @ElementCollection
    @CollectionTable(
            name = "course_items",
            joinColumns = @JoinColumn(name = "course_id")
    )
    @Column(name = "item_name", nullable = false)
    @Builder.Default
    private List<EquipmentRequirement> equipmentRequirements = new ArrayList<>();

    @Column(nullable = false)
    private int plannedHours;

    @Column(nullable = false)
    private int requiredRoomCapacity;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private Status status;

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;

    @PrePersist
    @PreUpdate
    private void normalize() {
        if (code != null) {
            code = code.trim();
        }
        if (title != null) {
            title = title.trim();
        }
        if (groupCodes != null) {
            groupCodes.replaceAll(c -> c == null ? null : c.trim());
        }
        if (equipmentRequirements != null) {
            for (EquipmentRequirement req : equipmentRequirements) {
                if (req.getName() != null) {
                    req.setName(req.getName().trim().toLowerCase());
                }
            }
        }
    }

    @Embeddable
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class EquipmentRequirement {
        @Column(name = "item_name", nullable = false, length = 128)
        private String name;

        @Column(name = "item_qty", nullable = false)
        private int quantity;
    }
}
