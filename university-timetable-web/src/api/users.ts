import {http} from './http';
import type {
    CreateUserRequest,
    CreateUserResult,
    SetPasswordRequest,
    UpdateUserRequest,
    UserResponse,
} from '@/types/auth';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/users';

export const usersApi = {
    create(body: CreateUserRequest) {
        return http<CreateUserResult>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    getAll() {
        return http<UserResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<UserResponse>(`${BASE_PATH}/${id}`);
    },

    update(id: UUID, body: UpdateUserRequest) {
        return http<UserResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    setPassword(id: UUID, body: SetPasswordRequest) {
        return http<UserResponse>(`${BASE_PATH}/${id}/password`, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    deactivate(id: UUID) {
        return http<UserResponse>(`${BASE_PATH}/${id}/deactivate`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<UserResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
        });
    },
};
