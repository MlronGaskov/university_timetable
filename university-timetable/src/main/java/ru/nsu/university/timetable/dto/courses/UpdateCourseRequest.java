package ru.nsu.university.timetable.dto.courses;

import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import ru.nsu.university.timetable.domain.ActivityForm;

import java.util.Set;
import java.util.UUID;

public record UpdateCourseRequest(

        @NotBlank
        @Size(max = 256)
        String title,

        @Size(max = 128)
        String department,

        @NotNull
        Set<ActivityForm> activityForms,

        @Min(0)
        int lectureHours,

        @Min(0)
        int seminarHours,

        @Min(0)
        int labHours,

        @Size(max = 512)
        String roomRequirements,

        Set<UUID> equipmentRequirementIds
) {
}
