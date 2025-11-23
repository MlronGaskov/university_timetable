package ru.nsu.university.timetable.domain;

import jakarta.persistence.*;
import lombok.*;

import java.util.UUID;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
@Entity
@Table(name = "room_equipment",
       uniqueConstraints = @UniqueConstraint(
               name = "uk_room_equipment_room_name",
               columnNames = {"room_id", "name"}))
public class RoomEquipment {
    @Id
    @GeneratedValue
    private UUID id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "room_id", nullable = false)
    private Room room;

    @Column(nullable = false, length = 128)
    private String name;

    @Column(nullable = false)
    private int quantity;
}
