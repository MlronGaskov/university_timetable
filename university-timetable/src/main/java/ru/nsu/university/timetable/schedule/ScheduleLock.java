package ru.nsu.university.timetable.schedule;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import org.springframework.context.annotation.EnableMBeanExport;

import java.time.Instant;
import java.util.UUID;

@Entity
@Table(name = "schedule_locks")
public class ScheduleLock {

    @Getter
    @Id
    @Column(columnDefinition = "uuid")
    private UUID versionId;

    @Column(columnDefinition = "uuid")
    private UUID ownerUserId;

    private Instant acquiredAt;
    private Instant expiresAt;

    public ScheduleLock() {
    }


}
