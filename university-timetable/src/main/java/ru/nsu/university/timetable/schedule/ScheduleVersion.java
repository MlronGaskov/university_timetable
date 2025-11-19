package ru.nsu.university.timetable.schedule;

import jakarta.persistence.*;
import lombok.Setter;

import java.time.Instant;
import java.util.UUID;

// Пока предполагаем, что Semester будет в пакете semester.
// Надо поправить импорт, когда появится сущность.
//import ru.nsu.university.timetable.semester.Semester;

@Entity
@Table(name = "schedule_versions")
public class ScheduleVersion {

    @Id
    @GeneratedValue
    @Column(columnDefinition = "uuid")
    private UUID id;

    @Setter
    private String name;

    //TODO Implement use-cases
//    @ManyToOne(fetch = FetchType.LAZY)
//    @JoinColumn(name = "semester_id", nullable = false)
//    private Semester semester;

    @Enumerated(EnumType.STRING)
    private ScheduleVersionStatus status = ScheduleVersionStatus.DRAFT;

    private boolean published;

    @Embedded
    private ScheduleQualityMetrics qualityMetrics;

    private Instant createdAt;

    @Column(columnDefinition = "uuid")
    private UUID createdByUserId;

    private Instant updatedAt;

    @Column(columnDefinition = "uuid")
    private UUID updatedByUserId;

    public ScheduleVersion() {
    }

    public UUID getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    //TODO Implement use-cases
//    public Semester getSemester() {
//        return semester;
//    }

    public ScheduleVersionStatus getStatus() {
        return status;
    }

    public boolean isPublished() {
        return published;
    }

    public ScheduleQualityMetrics getQualityMetrics() {
        return qualityMetrics;
    }

    public Instant getCreatedAt() {
        return createdAt;
    }

    public UUID getCreatedByUserId() {
        return createdByUserId;
    }

    public Instant getUpdatedAt() {
        return updatedAt;
    }

    public UUID getUpdatedByUserId() {
        return updatedByUserId;
    }
}
