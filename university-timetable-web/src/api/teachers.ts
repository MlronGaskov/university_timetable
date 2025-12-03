import {http} from './http';
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

    update(id: UUID, body: UpdateTeacherRequest) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
        });
    },

    updateWorkingHours(id: UUID, body: UpdateWorkingHoursRequest) {
        return http<TeacherResponse>(`${BASE_PATH}/${id}/working-hours`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },
};
