import {http} from './http';
import type {
    ScheduleResponse,
    ScheduleSummaryResponse,
} from '@/types/schedules';
import type {UUID} from '@/types/common';

export const schedulesApi = {
    getForSemester(semesterId: UUID) {
        return http<ScheduleSummaryResponse[]>(
            `/api/semesters/${semesterId}/schedules`,
        );
    },

    getById(scheduleId: UUID) {
        return http<ScheduleResponse>(`/api/schedules/${scheduleId}`);
    },

    generateForSemester(semesterId: UUID) {
        return http<ScheduleResponse>(
            `/api/semesters/${semesterId}/schedules/generate`,
            {
                method: 'POST',
            },
        );
    },
};
