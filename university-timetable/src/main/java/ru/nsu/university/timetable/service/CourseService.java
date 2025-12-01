package ru.nsu.university.timetable.service;

import ru.nsu.university.timetable.domain.ActivityForm;
import ru.nsu.university.timetable.dto.courses.CourseDto;
import ru.nsu.university.timetable.dto.courses.CourseListItemDto;
import ru.nsu.university.timetable.dto.courses.CreateCourseRequest;
import ru.nsu.university.timetable.dto.courses.UpdateCourseRequest;

import java.util.List;
import java.util.UUID;

public interface CourseService {

    CourseDto create(CreateCourseRequest request);

    CourseDto getById(UUID id);

    List<CourseListItemDto> list(String department,
                                 ActivityForm activityForm);

    CourseDto update(UUID id, UpdateCourseRequest request);

    void archiveOrDelete(UUID id, boolean hardDeleteRequested);
}
