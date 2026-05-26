package ru.nsu.university.timetable.catalog.group;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import ru.nsu.university.timetable.catalog.common.Status;
import ru.nsu.university.timetable.catalog.group.dto.UpdateGroupRequest;
import ru.nsu.university.timetable.user.student.StudentRepository;
import ru.nsu.university.timetable.web.ResourceConflictException;
import ru.nsu.university.timetable.schedule.timetable.regeneration.ScheduleRegenerationService;

import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class GroupServiceOptimisticLockingTest {
    @Mock
    private GroupRepository groupRepository;

    @Mock
    private StudentRepository studentRepository;

    @Mock
    private ScheduleRegenerationService scheduleRegenerationService;

    @InjectMocks
    private GroupService service;

    @Test
    void update_rejectsStaleVersionBeforeMutatingEntity() {
        UUID id = UUID.randomUUID();
        Group group = Group.builder()
                .name("Old")
                .code("G1")
                .size(10)
                .status(Status.ACTIVE)
                .build();
        group.setId(id);
        group.setVersion(2L);

        when(groupRepository.findById(id)).thenReturn(Optional.of(group));

        assertThrows(
                ResourceConflictException.class,
                () -> service.update(id, 1L, new UpdateGroupRequest("New", null, null))
        );

        assertEquals("Old", group.getName());
        verify(groupRepository, never()).saveAndFlush(any());
    }

    @Test
    void update_allowsMatchingVersionAndFlushes() {
        UUID id = UUID.randomUUID();
        Group group = Group.builder()
                .name("Old")
                .code("G1")
                .size(10)
                .status(Status.ACTIVE)
                .build();
        group.setId(id);
        group.setVersion(2L);

        when(groupRepository.findById(id)).thenReturn(Optional.of(group));
        when(groupRepository.saveAndFlush(group)).thenAnswer(invocation -> invocation.getArgument(0));

        var response = service.update(id, 2L, new UpdateGroupRequest("New", null, 12));

        assertEquals("New", response.name());
        assertEquals(12, response.size());
        verify(groupRepository).saveAndFlush(group);
        verify(scheduleRegenerationService).regenerateAfterGroupChanged("G1");
    }
}
