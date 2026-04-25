import {http, ifMatchHeaders} from './http';
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

    update(id: UUID, body: UpdateUserRequest, version: number) {
        return http<UserResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },

    setPassword(id: UUID, body: SetPasswordRequest, version: number) {
        return http<UserResponse>(`${BASE_PATH}/${id}/password`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },

    deactivate(id: UUID, version: number) {
        return http<UserResponse>(`${BASE_PATH}/${id}/deactivate`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    activate(id: UUID, version: number) {
        return http<UserResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },
};
