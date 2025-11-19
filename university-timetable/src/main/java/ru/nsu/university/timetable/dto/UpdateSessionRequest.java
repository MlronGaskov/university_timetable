package ru.nsu.university.timetable.dto;

import lombok.Data;

import java.time.LocalDate;
import java.util.UUID;

@Data
public class UpdateSessionRequest {

    private UUID teacherId;
    private UUID roomId;
    private LocalDate date;
    private Integer slotIndex;
}
