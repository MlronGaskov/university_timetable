import {http, ifMatchHeaders} from './http';
import type {
    CreateTeacherRequest,
    TeacherResponse,
    UpdateTeacherRequest,
    UpdateWorkingHoursRequest,
} from '@/types/teachers';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/teachers';

export const teachersApi = {
    getAll() {
        return http<TeacherResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}`);
    },

    create(body: CreateTeacherRequest) {
        return http<TeacherResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateTeacherRequest, version: number) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID, version: number) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    activate(id: UUID, version: number) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    updateWorkingHours(
        id: UUID,
        body: UpdateWorkingHoursRequest,
        version: number
    ) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}/working-hours`, {
            method: 'PUT',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },
};
