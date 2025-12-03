import {http} from './http';
import type {
    GroupResponse,
    CreateGroupRequest,
    UpdateGroupRequest,
} from '@/types/group';
import type {UUID} from '@/types/common';

export const groupsApi = {
    getAll() {
        return http<GroupResponse[]>('/api/groups');
    },

    getOne(id: UUID) {
        return http<GroupResponse>(`/api/groups/${id}`);
    },

    create(body: CreateGroupRequest) {
        return http<GroupResponse>('/api/groups', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateGroupRequest) {
        return http<GroupResponse>(`/api/groups/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<GroupResponse>(`/api/groups/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<GroupResponse>(`/api/groups/${id}/activate`, {
            method: 'POST',
        });
    },

    addStudent(groupId: UUID, studentId: UUID) {
        return http<GroupResponse>(`/api/groups/${groupId}/students/${studentId}`, {
            method: 'POST',
        });
    },

    removeStudent(groupId: UUID, studentId: UUID) {
        return http<GroupResponse>(`/api/groups/${groupId}/students/${studentId}`, {
            method: 'DELETE',
        });
    },
};
