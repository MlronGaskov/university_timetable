import React, {
    createContext,
    useContext,
    useEffect,
    useMemo,
    useState,
    type ReactNode,
} from 'react';
import {authApi} from '@/api/auth';
import {setAuthToken} from '@/api/http';
import type {Role} from '@/types/auth';

const STORAGE_KEY_TOKEN = 'authToken';

interface AuthTokenPayload {
    role?: Role;
    userId?: string;
    teacherId?: string;
    studentId?: string;
    sub?: string;
    [key: string]: unknown;
}

export interface AuthContextValue {
    token: string | null;
    role: Role | null;
    userId: string | null;
    teacherId: string | null;
    studentId: string | null;
    isAuthenticated: boolean;
    login: (login: string, password: string) => Promise<void>;
    logout: () => void;
}

const AuthContext = createContext<AuthContextValue | undefined>(undefined);

function decodePayload(token: string | null): AuthTokenPayload | null {
    if (!token) return null;
    try {
        const parts = token.split('.');
        if (parts.length < 2) return null;
        const payload = parts[1];
        const base64 = payload.replace(/-/g, '+').replace(/_/g, '/');
        const json = atob(base64);
        return JSON.parse(json) as AuthTokenPayload;
    } catch {
        return null;
    }
}

export const AuthProvider: React.FC<{ children: ReactNode }> = ({children}) => {
    const [token, setToken] = useState<string | null>(() => {
        if (typeof window === 'undefined') return null;
        return localStorage.getItem(STORAGE_KEY_TOKEN);
    });

    const [payload, setPayload] = useState<AuthTokenPayload | null>(() =>
        decodePayload(
            typeof window !== 'undefined'
                ? localStorage.getItem(STORAGE_KEY_TOKEN)
                : null
        )
    );

    useEffect(() => {
        setAuthToken(token);

        if (typeof window === 'undefined') return;

        if (token) {
            localStorage.setItem(STORAGE_KEY_TOKEN, token);
            setPayload(decodePayload(token));
        } else {
            localStorage.removeItem(STORAGE_KEY_TOKEN);
            setPayload(null);
        }
    }, [token]);

    const login = async (loginStr: string, password: string) => {
        const {token: newToken} = await authApi.login({
            login: loginStr,
            password,
        });
        setToken(newToken);
    };

    const logout = () => {
        setToken(null);
    };

    const role: Role | null =
        payload?.role === 'ADMIN' || payload?.role === 'TEACHER' || payload?.role === 'STUDENT'
            ? payload.role
            : null;

    const value = useMemo<AuthContextValue>(
        () => ({
            token,
            role,
            userId: (payload?.userId as string | undefined) ?? null,
            teacherId: (payload?.teacherId as string | undefined) ?? null,
            studentId: (payload?.studentId as string | undefined) ?? null,
            isAuthenticated: Boolean(token),
            login,
            logout,
        }),
        [token, role, payload]
    );

    return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
};

export const useAuth = (): AuthContextValue => {
    const ctx = useContext(AuthContext);
    if (!ctx) {
        throw new Error('useAuth must be used within AuthProvider');
    }
    return ctx;
};
