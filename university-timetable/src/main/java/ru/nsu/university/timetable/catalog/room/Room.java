package ru.nsu.university.timetable.catalog.room;

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
        name = "rooms",
        uniqueConstraints = @UniqueConstraint(
                name = "uk_rooms_building_number",
                columnNames = {"building", "number"}
        )
)
@EntityListeners(AuditingEntityListener.class)
public class Room {
    @Id
    @GeneratedValue
    private UUID id;

    @Column(nullable = false, length = 64)
    private String building;

    @Column(nullable = false, length = 64)
    private String number;

    @Column(nullable = false)
    private int capacity;

    @Enumerated(EnumType.STRING)
    @Column(nullable = false, length = 16)
    private Status status;

    @ElementCollection
    @CollectionTable(
            name = "room_items",
            joinColumns = @JoinColumn(name = "room_id")
    )
    @Builder.Default
    private List<RoomItem> items = new ArrayList<>();

    @CreatedDate
    @Column(nullable = false, updatable = false)
    private Instant createdAt;

    @LastModifiedDate
    @Column(nullable = false)
    private Instant updatedAt;

    @Embeddable
    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoomItem {
        @Column(name = "item_name", nullable = false, length = 128)
        private String name;

        @Column(name = "item_qty", nullable = false)
        private int quantity;
    }
}
