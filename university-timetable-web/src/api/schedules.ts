import {http} from './http';
import type {
    ScheduleResponse,
    ScheduleSummaryResponse,
} from '@/types/schedules';
import type {UUID} from '@/types/common';

export const schedulesApi = {
    getForSemester(semesterCode: string) {
        return http<ScheduleSummaryResponse[]>(
            `/api/semesters/${semesterCode}/schedules`,
        );
    },

    getById(scheduleId: UUID) {
        return http<ScheduleResponse>(`/api/schedules/${scheduleId}`);
    },

    generateForSemester(semesterCode: string) {
        return http<ScheduleResponse>(
            `/api/semesters/${semesterCode}/schedules/generate`,
            {
                method: 'POST',
            },
        );
    },
};
