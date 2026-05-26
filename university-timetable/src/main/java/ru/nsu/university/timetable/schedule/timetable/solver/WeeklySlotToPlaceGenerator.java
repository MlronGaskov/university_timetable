package ru.nsu.university.timetable.schedule.timetable.solver;

import org.springframework.stereotype.Component;
import ru.nsu.university.timetable.schedule.course.Course;
import ru.nsu.university.timetable.solver.dto.SolverRequest;

import java.util.ArrayList;
import java.util.List;

@Component
public class WeeklySlotToPlaceGenerator {

    public List<SolverRequest.SlotToPlaceDto> generate(List<Course> courses) {
        if (courses == null || courses.isEmpty()) {
            return List.of();
        }

        List<SolverRequest.SlotToPlaceDto> result = new ArrayList<>();
        for (Course course : courses) {
            result.addAll(generateForCourse(course));
        }
        return result;
    }

    public List<SolverRequest.SlotToPlaceDto> generateForCourse(Course course) {
        int occurrences = requiredWeeklyPairCount(course.getPlannedHours());
        List<SolverRequest.EquipmentRequirementDto> equipmentRequirements = safeList(course.getEquipmentRequirements()).stream()
                .map(req -> new SolverRequest.EquipmentRequirementDto(req.getName(), req.getQuantity()))
                .toList();

        List<SolverRequest.SlotToPlaceDto> result = new ArrayList<>(occurrences);
        for (int i = 1; i <= occurrences; i++) {
            result.add(new SolverRequest.SlotToPlaceDto(
                    buildRequestId(course.getCode(), i),
                    course.getCode(),
                    course.getTeacherId(),
                    safeList(course.getGroupCodes()),
                    course.getRequiredRoomCapacity(),
                    equipmentRequirements
            ));
        }
        return result;
    }

    int requiredWeeklyPairCount(int plannedHours) {
        if (plannedHours <= 0) {
            return 0;
        }
        return (plannedHours + 1) / 2;
    }

    private String buildRequestId(String courseCode, int occurrence) {
        return courseCode + "#" + occurrence;
    }

    private static <T> List<T> safeList(List<T> list) {
        return list == null ? List.of() : list;
    }
}
