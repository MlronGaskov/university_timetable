import {http} from './http';
import type {
    PolicyResponse,
    CreatePolicyRequest,
    UpdatePolicyRequest,
} from '@/types/policy';
import type {UUID} from '@/types/common';

export const policiesApi = {
    getAll() {
        return http<PolicyResponse[]>('/api/policies');
    },

    getOne(id: UUID) {
        return http<PolicyResponse>(`/api/policies/${id}`);
    },

    getByName(name: string) {
        return http<PolicyResponse>(`/api/policies/by-name/${encodeURIComponent(name)}`);
    },

    create(body: CreatePolicyRequest) {
        return http<PolicyResponse>('/api/policies', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdatePolicyRequest) {
        return http<PolicyResponse>(`/api/policies/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    delete(id: UUID) {
        return http<void>(`/api/policies/${id}`, {
            method: 'DELETE',
        });
    },
};
