import {http} from './http';
import type {
    TeacherResponse,
    CreateTeacherRequest,
    UpdateTeacherRequest,
    UpdateWorkingHoursRequest,
} from '@/types/teacher';
import type {UUID} from '@/types/common';

export const teachersApi = {
    getAll() {
        return http<TeacherResponse[]>('/api/teachers');
    },

    getOne(id: UUID) {
        return http<TeacherResponse>(`/api/teachers/${id}`);
    },

    create(body: CreateTeacherRequest) {
        return http<TeacherResponse>('/api/teachers', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateTeacherRequest) {
        return http<TeacherResponse>(`/api/teachers/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<TeacherResponse>(`/api/teachers/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<TeacherResponse>(`/api/teachers/${id}/activate`, {
            method: 'POST',
        });
    },

    updateWorkingHours(id: UUID, body: UpdateWorkingHoursRequest) {
        return http<TeacherResponse>(`/api/teachers/${id}/working-hours`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },
};
