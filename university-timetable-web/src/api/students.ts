import {http, ifMatchHeaders} from './http';
import type {
    CreateStudentRequest,
    StudentResponse,
    UpdateStudentRequest,
} from '@/types/students';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/students';

export const studentsApi = {
    getAll() {
        return http<StudentResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<StudentResponse>(`${BASE_PATH}/${id}`);
    },

    create(body: CreateStudentRequest) {
        return http<StudentResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateStudentRequest, version: number) {
        return http<StudentResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID, version: number) {
        return http<StudentResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    activate(id: UUID, version: number) {
        return http<StudentResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },
};
