import type {Status, UUID} from './common';

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
    createdAt: string;
    updatedAt: string;
}
