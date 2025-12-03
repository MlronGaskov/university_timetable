import type {Status, UUID} from './common';

export type DayOfWeek =
    | 'MONDAY'
    | 'TUESDAY'
    | 'WEDNESDAY'
    | 'THURSDAY'
    | 'FRIDAY'
    | 'SATURDAY'
    | 'SUNDAY';

export interface WorkingIntervalDto {
    day: DayOfWeek;
    startTime: string;
    endTime: string;
}

export interface CreateTeacherRequest {
    teacherId: string;
    fullName: string;
}

export interface UpdateTeacherRequest {
    teacherId?: string;
    fullName?: string;
    preferredWorkingHours?: WorkingIntervalDto[];
}

export interface UpdateWorkingHoursRequest {
    preferredWorkingHours: WorkingIntervalDto[];
}

export interface TeacherResponse {
    id: UUID;
    teacherId: string;
    fullName: string;
    status: Status;
    preferredWorkingHours: WorkingIntervalDto[];
    createdAt: string;
    updatedAt: string;
}
