package ru.nsu.university.timetable.schedule.timetable;

import jakarta.persistence.*;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.time.Instant;

@Getter
@Setter
@NoArgsConstructor
@Entity
@Table(name = "schedule_generation_locks")
public class ScheduleGenerationLock {
    @Id
    @Column(name = "semester_code", nullable = false, length = 64)
    private String semesterCode;

    @Version
    @Column(nullable = false)
    private long version;

    @Column(name = "updated_at", nullable = false)
    private Instant updatedAt = Instant.now();

    public ScheduleGenerationLock(String semesterCode) {
        this.semesterCode = semesterCode;
        this.updatedAt = Instant.now();
    }

    @PrePersist
    @PreUpdate
    void touch() {
        updatedAt = Instant.now();
    }
}
