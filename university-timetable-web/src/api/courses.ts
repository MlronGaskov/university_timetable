import {http} from './http';
import type {CourseResponse, CreateCourseRequest, UpdateCourseRequest,} from '@/types/courses';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/courses';

export const coursesApi = {
    getAll() {
        return http<CourseResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<CourseResponse>(`${BASE_PATH}/${id}`);
    },

    create(body: CreateCourseRequest) {
        return http<CourseResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateCourseRequest) {
        return http<CourseResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<CourseResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<CourseResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
        });
    },

    addGroup(courseId: UUID, groupCode: string) {
        return http<CourseResponse>(`${BASE_PATH}/${courseId}/groups/${groupCode}`, {
            method: 'POST',
        });
    },

    removeGroup(courseId: UUID, groupCode: string) {
        return http<CourseResponse>(`${BASE_PATH}/${courseId}/groups/${groupCode}`, {
            method: 'DELETE',
        });
    },
};
