import {TeacherDto} from '@/types';
import {api} from './client';

export type CreateTeacherReq = { fullName: string };
export type UpdateTeacherReq = { fullName?: string };

export const TeachersApi = {
    list: () => api<TeacherDto[]>('/api/teachers'),
    get: (id: string) => api<TeacherDto>(`/api/teachers/${id}`),

    create: (body: CreateTeacherReq) =>
        api<TeacherDto>('/api/teachers', {
            method: 'POST',
            body: JSON.stringify(body),
        }),

    update: (id: string, body: UpdateTeacherReq) =>
        api<TeacherDto>(`/api/teachers/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        }),

    archive: (id: string) =>
        api<TeacherDto>(`/api/teachers/${id}/archive`, {method: 'POST'}),
};
