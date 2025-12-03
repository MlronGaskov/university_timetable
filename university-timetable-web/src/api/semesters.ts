import {http} from './http';
import type {CreateSemesterRequest, SemesterResponse, UpdateSemesterRequest,} from '@/types/semesters';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/semesters';

export const semestersApi = {
    getAll() {
        return http<SemesterResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<SemesterResponse>(`${BASE_PATH}/${id}`);
    },

    create(body: CreateSemesterRequest) {
        return http<SemesterResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateSemesterRequest) {
        return http<SemesterResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<SemesterResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<SemesterResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
        });
    },
};
