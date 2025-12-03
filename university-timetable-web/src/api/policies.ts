import {http} from './http';
import type {CreatePolicyRequest, PolicyResponse, UpdatePolicyRequest,} from '@/types/policies';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/policies';

export const policiesApi = {
    getAll() {
        return http<PolicyResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<PolicyResponse>(`${BASE_PATH}/${id}`);
    },

    getByName(name: string) {
        return http<PolicyResponse>(`${BASE_PATH}/by-name/${encodeURIComponent(name)}`);
    },

    create(body: CreatePolicyRequest) {
        return http<PolicyResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdatePolicyRequest) {
        return http<PolicyResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    delete(id: UUID) {
        return http<void>(`${BASE_PATH}/${id}`, {
            method: 'DELETE',
        });
    },
};
