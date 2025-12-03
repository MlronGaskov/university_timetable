import type {Status, UUID} from './common';

export interface CreateSemesterRequest {
    code: string;
    startAt: string;
    endAt: string;
    policyId: UUID;
    courseIds?: UUID[] | null;
    roomCodes?: string[] | null;
}

export interface UpdateSemesterRequest {
    code?: string;
    startAt?: string;
    endAt?: string;
    policyId?: UUID;
    courseIds?: UUID[] | null;
    roomCodes?: string[] | null;
}

export interface SemesterResponse {
    id: UUID;
    code: string;
    startAt: string;
    endAt: string;
    status: Status;
    policyId: UUID;
    courseIds: UUID[];
    roomCodes: string[];
    createdAt: string;
    updatedAt: string;
}
