package ru.nsu.university.timetable.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import ru.nsu.university.timetable.dto.*;
import ru.nsu.university.timetable.service.*;

import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

@RestController
@RequestMapping("/api/schedules")
@RequiredArgsConstructor
public class ScheduleController {

    private final ScheduleGenerationService generationService;
    private final LocalRecomputeService localRecomputeService;
    private final ManualEditService manualEditService;
    private final ScheduleViewService viewService;
    private final ScheduleVersionService versionService;
    private final ScheduleLockService lockService;

    // UC-SCHED-1: Generate schedule
    @PostMapping("/generate")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public GenerateScheduleResponse generate(@RequestBody GenerateScheduleRequest request) {
        return generationService.generate(request);
    }

    // UC-SCHED-2: Local recompute
    @PostMapping("/{versionId}/local-recompute")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public Object localRecompute(
            @PathVariable UUID versionId,
            @RequestBody LocalRecomputeRequest request
    ) {
        request.setVersionId(versionId);
        return localRecomputeService.localRecompute(request);
    }

    // UC-SCHED-3: Locks
    @PostMapping("/{versionId}/lock")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public void acquireLock(@PathVariable UUID versionId) {
        lockService.acquire(versionId);
    }

    @DeleteMapping("/{versionId}/lock")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public void releaseLock(@PathVariable UUID versionId) {
        lockService.release(versionId);
    }

    // UC-SCHED-3: Manual edit
    @PutMapping("/{versionId}/sessions/{sessionId}")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public ClassSessionDto updateSession(
            @PathVariable UUID versionId,
            @PathVariable UUID sessionId,
            @RequestBody UpdateSessionRequest request
    ) {
        return manualEditService.updateSession(versionId, sessionId, request);
    }

    @PostMapping("/{versionId}/sessions/{sessionId}/assist")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public AssistedPlacementResponse assist(
            @PathVariable UUID versionId,
            @PathVariable UUID sessionId,
            @RequestBody AssistedPlacementRequest request
    ) {
        request.setSessionId(sessionId);
        return manualEditService.assistedPlacement(versionId, request);
    }

    // UC-SCHED-4: View schedule slices
    @GetMapping("/{versionId}/slice")
    @PreAuthorize("isAuthenticated()")
    public ScheduleSliceResponse getSlice(
            @PathVariable UUID versionId,
            @RequestParam SliceType type,
            @RequestParam UUID entityId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        ScheduleSliceRequest req = new ScheduleSliceRequest();
        req.setType(type);
        req.setEntityId(entityId);
        req.setFromDate(fromDate);
        req.setToDate(toDate);
        return viewService.getSlice(versionId, req);
    }

    // UC-SCHED-5: Archive / restore versions + list
    @PostMapping("/{versionId}/archive")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public ScheduleVersionDto archive(@PathVariable UUID versionId) {
        return versionService.archive(versionId);
    }

    @PostMapping("/{versionId}/restore")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public ScheduleVersionDto restore(@PathVariable UUID versionId) {
        return versionService.restore(versionId);
    }

    @GetMapping("/versions")
    @PreAuthorize("hasAnyRole('ADMIN', 'SCHEDULER')")
    public List<ScheduleVersionDto> list(@RequestParam UUID semesterId) {
        return versionService.listBySemester(semesterId);
    }
}
