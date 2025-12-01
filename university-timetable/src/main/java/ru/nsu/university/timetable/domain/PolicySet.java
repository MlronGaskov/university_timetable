package ru.nsu.university.timetable.domain;

import jakarta.persistence.*;
import lombok.*;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.Instant;
import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "policy_sets")
@EntityListeners(AuditingEntityListener.class)
public class PolicySet {

    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false)
    private Long version;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private PolicySetStatus status;

    @Lob
    @Column(name = "grid_json", nullable = false)
    private String gridJson;

    @Lob
    @Column(name = "breaks_json", nullable = false)
    private String breaksJson;

    @Lob
    @Column(name = "limits_json", nullable = false)
    private String limitsJson;

    @Lob
    @Column(name = "travel_matrix_json", nullable = false)
    private String travelMatrixJson;

    @Lob
    @Column(name = "weights_json", nullable = false)
    private String weightsJson;

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;

    @Column(length = 64)
    private String createdBy;
}
