package ru.nsu.university.timetable.schedule.semester;

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
        name = "semesters",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_semesters_code",
                columnNames = {"code"}
        )
)
@EntityListeners(AuditingEntityListener.class)
public class Semester {
    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, length = 64)
    private String code;

    @Column(nullable = false)
    private Instant startAt;

    @Column(nullable = false)
    private Instant endAt;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private Status status;

    @ElementCollection
    @CollectionTable(
            name = "semester_courses",
            joinColumns = @JoinColumn(name = "semester_id")
    )
    @Column(name = "course_id", nullable = false)
    @Builder.Default
    private List<UUID> courseIds = new ArrayList<>();

    @Column(name = "policy_id")
    private UUID policyId;

    @ElementCollection
    @CollectionTable(
            name = "semester_rooms",
            joinColumns = @JoinColumn(name = "semester_id")
    )
    @Column(name = "room_code", nullable = false, length = 64)
    @Builder.Default
    private List<String> roomCodes = new ArrayList<>();

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
    }
}
