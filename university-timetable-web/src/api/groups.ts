import {GroupDto, SubgroupDto} from '@/types';
import {api} from './client';

export type CreateGroupReq = { name: string; code: string; size: number };
export type UpdateGroupReq = { name?: string; code?: string; size?: number };

export type CreateSubgroupReq = { name: string; size: number };
export type UpdateSubgroupReq = { name?: string; size?: number };

export const GroupsApi = {
    list: () => api<GroupDto[]>('/api/groups'),
    get: (id: string) => api<GroupDto>(`/api/groups/${id}`),

    create: (body: CreateGroupReq) =>
        api<GroupDto>('/api/groups', {
            method: 'POST',
            body: JSON.stringify(body),
        }),

    update: (id: string, body: UpdateGroupReq) =>
        api<GroupDto>(`/api/groups/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        }),

    archive: (id: string) =>
        api<GroupDto>(`/api/groups/${id}/archive`, {method: 'POST'}),

    createSubgroup: (groupId: string, body: CreateSubgroupReq) =>
        api<SubgroupDto>(`/api/groups/${groupId}/subgroups`, {
            method: 'POST',
            body: JSON.stringify(body),
        }),

    updateSubgroup: (id: string, body: UpdateSubgroupReq) =>
        api<SubgroupDto>(`/api/subgroups/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        }),

    archiveSubgroup: (id: string) =>
        api<SubgroupDto>(`/api/subgroups/${id}/archive`, {method: 'POST'}),
};
