import {http} from './http';
import type {CreateGroupRequest, GroupResponse, UpdateGroupRequest,} from '@/types/groups';
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

    update(id: UUID, body: UpdateGroupRequest) {
        return http<GroupResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<GroupResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<GroupResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
        });
    },

    addStudent(groupId: UUID, studentId: UUID) {
        return http<GroupResponse>(`${BASE_PATH}/${groupId}/students/${studentId}`, {
            method: 'POST',
        });
    },

    removeStudent(groupId: UUID, studentId: UUID) {
        return http<GroupResponse>(`${BASE_PATH}/${groupId}/students/${studentId}`, {
            method: 'DELETE',
        });
    },
};
