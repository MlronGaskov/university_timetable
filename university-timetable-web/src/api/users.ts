import {http} from './http';
import type {
    CreateUserRequest,
    CreateUserResult,
    UpdateUserRequest,
    SetPasswordRequest,
    UserResponse,
} from '@/types/auth';
import type {UUID} from '@/types/common';

export const usersApi = {
    create(body: CreateUserRequest) {
        return http<CreateUserResult>('/api/users', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    getAll() {
        return http<UserResponse[]>('/api/users');
    },

    getOne(id: UUID) {
        return http<UserResponse>(`/api/users/${id}`);
    },

    update(id: UUID, body: UpdateUserRequest) {
        return http<UserResponse>(`/api/users/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    setPassword(id: UUID, body: SetPasswordRequest) {
        return http<UserResponse>(`/api/users/${id}/password`, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    deactivate(id: UUID) {
        return http<UserResponse>(`/api/users/${id}/deactivate`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<UserResponse>(`/api/users/${id}/activate`, {
            method: 'POST',
        });
    },
};
