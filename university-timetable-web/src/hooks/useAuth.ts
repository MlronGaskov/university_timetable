import {useMemo, useState} from 'react';
import {loginRequest, decodeJwt} from '@api/client';

type JwtAuthority = string | { authority?: string };
type TokenPayload = {
    role?: 'ADMIN' | 'TEACHER' | 'STUDENT';
    authorities?: JwtAuthority[];
    exp: number; iat: number; sub: string;
};

export function useAuth() {
    const [token, setToken] = useState<string | null>(() => localStorage.getItem('jwt'));

    const payload = useMemo(() => {
        if (!token) return null;
        return decodeJwt<TokenPayload>(token);
    }, [token]);

    const authorities = payload?.authorities ?? [];
    const isAdminByAuthorities = authorities.some((a) => {
        const v = typeof a === 'string' ? a : a.authority;
        return v === 'ROLE_ADMIN' || v === 'ADMIN' || v?.includes('ADMIN');
    });

    const isAdminByRole = payload?.role === 'ADMIN';

    const isAuthed = !!token;
    const isAdmin = isAdminByAuthorities || isAdminByRole;
    const role = isAdmin ? 'ADMIN' : (payload?.role ?? "USER");

    const headers = useMemo(
        () => ({'Content-Type': 'application/json', ...(token ? {Authorization: `Bearer ${token}`} : {})}),
        [token]
    );

    async function login(login: string, password: string) {
        const {token} = await loginRequest(login, password);
        localStorage.setItem('jwt', token);
        setToken(token);
    }

    function logout() {
        localStorage.removeItem('jwt');
        setToken(null);
        window.location.replace('/login');
    }

    return {token, isAuthed, isAdmin, role: role ?? null, headers, login, logout};
}
