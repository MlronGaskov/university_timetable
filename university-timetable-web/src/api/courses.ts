import {http, ifMatchHeaders} from './http';
import type {
    CourseResponse,
    CreateCourseRequest,
    UpdateCourseRequest,
} from '@/types/courses';
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

    update(id: UUID, body: UpdateCourseRequest, version: number) {
        return http<CourseResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID, version: number) {
        return http<CourseResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    activate(id: UUID, version: number) {
        return http<CourseResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    addGroup(courseId: UUID, groupCode: string, version: number) {
        return http<CourseResponse>(
            `${BASE_PATH}/${courseId}/groups/${encodeURIComponent(groupCode)}`,
            {
                method: 'POST',
                headers: ifMatchHeaders(version),
            }
        );
    },

    removeGroup(courseId: UUID, groupCode: string, version: number) {
        return http<CourseResponse>(
            `${BASE_PATH}/${courseId}/groups/${encodeURIComponent(groupCode)}`,
            {
                method: 'DELETE',
                headers: ifMatchHeaders(version),
            }
        );
    },
};
