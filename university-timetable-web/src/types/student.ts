import type {UUID, Instant, Status} from './common';

export interface CreateStudentRequest {
    fullName: string;
    studentId: string;
}

export interface UpdateStudentRequest {
    fullName?: string;
    studentId?: string;
}

export interface StudentResponse {
    id: UUID;
    fullName: string;
    studentId: string;
    status: Status;
    createdAt: Instant;
    updatedAt: Instant;
}
