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

    @Column(name = "policy_name", nullable = false, length = 128)
    private String policyName;

    @ElementCollection
    @CollectionTable(
            name = "semester_courses",
            joinColumns = @JoinColumn(name = "semester_id")
    )
    @Column(name = "course_code", nullable = false, length = 64)
    @Builder.Default
    private List<String> courseCodes = new ArrayList<>();

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
        if (policyName != null) {
            policyName = policyName.trim();
        }
        if (courseCodes != null) {
            courseCodes.replaceAll(s -> s == null ? null : s.trim());
        }
        if (roomCodes != null) {
            roomCodes.replaceAll(s -> s == null ? null : s.trim());
        }
    }
}
