import { UserDto, Role, Status } from '@/types';
import { api } from './client';

export type CreateUserReq = { login: string; email: string; role: Role; password?: string };
export type UpdateUserReq = { email?: string; role?: Role; status?: Status };

export const UsersApi = {
    list: () => api<UserDto[]>('/api/users'),
    create: (body: CreateUserReq) => api<UserDto>('/api/users', { method: 'POST', body: JSON.stringify(body) }),
    get: (id: string) => api<UserDto>(`/api/users/${id}`),
    update: (id: string, body: UpdateUserReq) => api<UserDto>(`/api/users/${id}`, { method: 'PUT', body: JSON.stringify(body) }),
    activate: (id: string) => api<UserDto>(`/api/users/${id}/activate`, { method: 'POST' }),
    deactivate: (id: string) => api<UserDto>(`/api/users/${id}/deactivate`, { method: 'POST' }),
};
