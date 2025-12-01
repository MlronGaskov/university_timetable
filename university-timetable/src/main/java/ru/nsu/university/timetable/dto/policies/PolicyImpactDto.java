package ru.nsu.university.timetable.dto.policies;

import lombok.Builder;
import lombok.Data;

import java.util.List;

@Data
@Builder
public class PolicyImpactDto {

    /**
     * Например, ID расписаний, которых коснётся изменение.
     * Сейчас — заглушка, TODO потом связать с реальными сущностями расписаний.
     */
    private List<String> affectedScheduleIds;

    private long affectedCount;
}
