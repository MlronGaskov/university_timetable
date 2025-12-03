import {http} from './http';
import type {
    StudentResponse,
    CreateStudentRequest,
    UpdateStudentRequest,
} from '@/types/student';
import type {UUID} from '@/types/common';

export const studentsApi = {
    getAll() {
        return http<StudentResponse[]>('/api/students');
    },

    getOne(id: UUID) {
        return http<StudentResponse>(`/api/students/${id}`);
    },

    create(body: CreateStudentRequest) {
        return http<StudentResponse>('/api/students', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateStudentRequest) {
        return http<StudentResponse>(`/api/students/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<StudentResponse>(`/api/students/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<StudentResponse>(`/api/students/${id}/activate`, {
            method: 'POST',
        });
    },
};
