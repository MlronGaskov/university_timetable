import type {Status, UUID} from './common';

export interface CreateSemesterRequest {
    code: string;
    startAt: string;
    endAt: string;
    policyName: string;
    courseCodes?: string[] | null;
    roomCodes?: string[] | null;
}

export interface UpdateSemesterRequest {
    code?: string;
    startAt?: string;
    endAt?: string;
    policyName?: string;
    courseCodes?: string[] | null;
    roomCodes?: string[] | null;
}

export interface SemesterResponse {
    id: UUID;
    code: string;
    startAt: string;
    endAt: string;
    status: Status;
    policyName: string;
    courseCodes: string[];
    roomCodes: string[];
    createdAt: string;
    updatedAt: string;
}