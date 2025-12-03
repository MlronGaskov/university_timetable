import type {UUID, Instant, Status} from './common';

export type Role = 'ADMIN' | 'TEACHER' | 'STUDENT';

export interface LoginRequest {
    login: string;
    password: string;
}

export interface TokenResponse {
    token: string;
}

export interface UserResponse {
    id: UUID;
    login: string;
    email: string;
    status: Status;
    role: Role;
    teacherId: UUID | null;
    teacherName: string | null;
    studentId: UUID | null;
    studentName: string | null;
    createdAt: Instant;
    updatedAt: Instant;
}

export interface CreateUserRequest {
    login: string;
    email: string;
    role: Role;
    password?: string | null;
    teacherId?: UUID | null;
    studentId?: UUID | null;
}

export interface UpdateUserRequest {
    email?: string;
    role?: Role;
    status?: Status;
    teacherId?: UUID | null;
    studentId?: UUID | null;
}

export interface SetPasswordRequest {
    newPassword: string;
}

export interface CreateUserResult {
    user: UserResponse;
    tempPassword: string | null;
}
