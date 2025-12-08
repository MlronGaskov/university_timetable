package ru.nsu.university.timetable.schedule.timetable;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;
import ru.nsu.university.timetable.schedule.semester.Semester;

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
        name = "schedules",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_schedules_semester_version",
                columnNames = {"semester_id", "version"}
        )
)
@EntityListeners(AuditingEntityListener.class)
public class Schedule {

    @Id
    @GeneratedValue
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "semester_id", nullable = false)
    private Semester semester;

    @Column(nullable = false)
    private int version;

    @Column(name = "evaluation_score")
    private Double evaluationScore;

    @OneToMany(
            mappedBy = "schedule",
            cascade = CascadeType.ALL,
            orphanRemoval = true
    )
    @Builder.Default
    private List<ScheduleSlot> slots = new ArrayList<>();

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;

    public void addSlot(ScheduleSlot slot) {
        slots.add(slot);
        slot.setSchedule(this);
    }
}
