import {http, ifMatchHeaders} from './http';
import type {
    CreateGroupRequest,
    GroupResponse,
    UpdateGroupRequest,
} from '@/types/groups';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/groups';

export const groupsApi = {
    getAll() {
        return http<GroupResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<GroupResponse>(`${BASE_PATH}/${id}`);
    },

    create(body: CreateGroupRequest) {
        return http<GroupResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateGroupRequest, version: number) {
        return http<GroupResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            headers: ifMatchHeaders(version),
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID, version: number) {
        return http<GroupResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    activate(id: UUID, version: number) {
        return http<GroupResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
            headers: ifMatchHeaders(version),
        });
    },

    addStudent(groupId: UUID, studentId: string, version: number) {
        return http<GroupResponse>(
            `${BASE_PATH}/${groupId}/students/${encodeURIComponent(studentId)}`,
            {
                method: 'POST',
                headers: ifMatchHeaders(version),
            }
        );
    },

    removeStudent(groupId: UUID, studentId: string, version: number) {
        return http<GroupResponse>(
            `${BASE_PATH}/${groupId}/students/${encodeURIComponent(studentId)}`,
            {
                method: 'DELETE',
                headers: ifMatchHeaders(version),
            }
        );
    },
};
