package ru.nsu.university.timetable.catalog.policy;

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
@Table(
        name = "policies",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_policies_name",
                columnNames = "name"
        )
)
@EntityListeners(AuditingEntityListener.class)
public class Policy {
    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, length = 128)
    private String name;

    @Column(name = "grid_json", nullable = false, columnDefinition = "TEXT")
    private String gridJson;

    @Column(name = "breaks_json", nullable = false, columnDefinition = "TEXT")
    private String breaksJson;

    @Column(name = "limits_json", nullable = false, columnDefinition = "TEXT")
    private String limitsJson;

    @Column(name = "travel_matrix_json", nullable = false, columnDefinition = "TEXT")
    private String travelMatrixJson;

    @Column(name = "weights_json", nullable = false, columnDefinition = "TEXT")
    private String weightsJson;

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;
}
