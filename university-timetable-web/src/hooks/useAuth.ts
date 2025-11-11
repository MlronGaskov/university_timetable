import {useMemo, useState} from 'react';
import {loginRequest} from '@api/client';

export function useAuth() {
    const [token, setToken] = useState<string | null>(() => localStorage.getItem('jwt'));
    const isAuthed = !!token;
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
    }

    return {token, isAuthed, headers, login, logout};
}
