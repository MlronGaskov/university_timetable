import {http} from './http';
import type {LoginRequest, TokenResponse} from '@/types/auth';

export const authApi = {
    login(body: LoginRequest) {
        return http<TokenResponse>('/api/auth/login', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },
};
