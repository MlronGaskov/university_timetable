import {StudentDto} from '@/types';
import {api} from './client';

export type CreateStudentReq = {
    fullName: string;
    studentId: string;
    groupId?: string | null;
};

export type UpdateStudentReq = {
    fullName?: string;
    studentId?: string;
    groupId?: string | null;
};

export const StudentsApi = {
    list: () => api<StudentDto[]>('/api/students'),
    get: (id: string) => api<StudentDto>(`/api/students/${id}`),

    create: (body: CreateStudentReq) =>
        api<StudentDto>('/api/students', {
            method: 'POST',
            body: JSON.stringify(body),
        }),

    update: (id: string, body: UpdateStudentReq) =>
        api<StudentDto>(`/api/students/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        }),

    archive: (id: string) =>
        api<StudentDto>(`/api/students/${id}/archive`, {method: 'POST'}),
};
