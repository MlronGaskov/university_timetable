package ru.nsu.university.timetable.user.teacher;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import ru.nsu.university.timetable.catalog.common.Status;

import java.time.Instant;
import java.util.HashSet;
import java.util.Set;
import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(
        name = "teachers",
        uniqueConstraints = {
                @UniqueConstraint(
                        name = "uk_teachers_full_name",
                        columnNames = "full_name"
                ),
                @UniqueConstraint(
                        name = "uk_teachers_teacher_id",
                        columnNames = "teacher_id"
                )
        }
)
@EntityListeners(AuditingEntityListener.class)
public class Teacher {
    @Id
    @GeneratedValue
    private UUID id;

    @Column(name = "teacher_id", nullable = false, length = 64)
    private String teacherId;

    @Column(name = "full_name", nullable = false, length = 128)
    private String fullName;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private Status status;

    @ElementCollection(fetch = FetchType.EAGER)
    @CollectionTable(
            name = "teacher_working_hours",
            joinColumns = @JoinColumn(name = "teacher_id_fk")
    )
    @Builder.Default
    private Set<TeacherWorkingHours> preferredWorkingHours = new HashSet<>();

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;
}
