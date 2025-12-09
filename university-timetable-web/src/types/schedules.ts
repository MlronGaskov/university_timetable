import type {UUID} from './common';
import type {DayOfWeek} from './teachers';

export type WeekPattern = 'EVERY_WEEK' | 'ODD_WEEKS' | 'EVEN_WEEKS';

export interface ScheduleSlotDto {
    id: UUID;
    courseCode: string;
    roomCode: string;
    dayOfWeek: DayOfWeek;
    startTime: string;
    endTime: string;
    validFrom: string;
    validUntil: string;
    weekPattern: WeekPattern;
}

export interface ScheduleSummaryResponse {
    id: UUID;
    semesterCode: string;
    version: number;
    evaluationScore: number | null;
    createdAt: string;
    updatedAt: string;
}

export interface ScheduleResponse {
    id: UUID;
    semesterCode: string;
    version: number;
    evaluationScore: number | null;
    createdAt: string;
    updatedAt: string;
    slots: ScheduleSlotDto[];
}
