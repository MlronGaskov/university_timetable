import type {Status, UUID} from './common';

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
    teacherId: string | null;
    teacherName: string | null;
    studentId: string | null;
    studentName: string | null;
    createdAt: string;
    updatedAt: string;
}

export interface CreateUserRequest {
    login: string;
    email: string;
    role: Role;
    password?: string | null;
    teacherId?: string | null;
    studentId?: string | null;
}

export interface UpdateUserRequest {
    email?: string;
    role?: Role;
    status?: Status;
    teacherId?: string | null;
    studentId?: string | null;
}

export interface SetPasswordRequest {
    newPassword: string;
}

export interface CreateUserResult {
    user: UserResponse;
    tempPassword: string | null;
}
