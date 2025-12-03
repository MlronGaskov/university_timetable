import type {UUID, Instant, Status} from './common';

export type DayOfWeek =
    | 'MONDAY'
    | 'TUESDAY'
    | 'WEDNESDAY'
    | 'THURSDAY'
    | 'FRIDAY'
    | 'SATURDAY'
    | 'SUNDAY';

export type LocalTime = string;

export interface WorkingIntervalDto {
    day: DayOfWeek;
    startTime: LocalTime;
    endTime: LocalTime;
}

export interface CreateTeacherRequest {
    fullName: string;
}

export interface UpdateTeacherRequest {
    fullName?: string;
    preferredWorkingHours?: WorkingIntervalDto[];
}

export interface UpdateWorkingHoursRequest {
    preferredWorkingHours: WorkingIntervalDto[];
}

export interface TeacherResponse {
    id: UUID;
    fullName: string;
    status: Status;
    preferredWorkingHours: WorkingIntervalDto[];
    createdAt: Instant;
    updatedAt: Instant;
}
