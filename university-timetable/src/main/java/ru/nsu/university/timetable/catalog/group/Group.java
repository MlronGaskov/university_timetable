package ru.nsu.university.timetable.catalog.group;

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
        name = "groups",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_groups_code",
                columnNames = "code"
        )
)
@EntityListeners(AuditingEntityListener.class)
public class Group {
    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, length = 128)
    private String name;

    @Column(nullable = false, length = 64)
    private String code;

    @Column(nullable = false)
    private int size;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private Status status;

    @ElementCollection
    @CollectionTable(
            name = "group_students",
            joinColumns = @JoinColumn(name = "group_id")
    )
    @Column(name = "student_id", nullable = false)
    @Builder.Default
    private List<UUID> studentIds = new ArrayList<>();

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;
}
