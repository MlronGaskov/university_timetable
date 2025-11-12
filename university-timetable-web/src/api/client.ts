export const API_BASE = import.meta.env.VITE_API_BASE ?? 'http://localhost:8080';

function authHeaders() {
    const token = localStorage.getItem('jwt');
    const headers: Record<string, string> = { 'Content-Type': 'application/json' };
    if (token) headers['Authorization'] = `Bearer ${token}`;
    return headers;
}

export async function api<T>(path: string, init?: RequestInit): Promise<T> {
    const res = await fetch(`${API_BASE}${path}`, {
        ...init,
        headers: { ...authHeaders(), ...(init?.headers || {}) },
    });

    if (res.status === 401) {
        localStorage.removeItem('jwt');
        window.location.replace('/login');
        throw new Error('Unauthorized');
    }

    if (!res.ok) {
        const ct = res.headers.get('content-type') || '';
        let message = res.statusText;

        if (ct.includes('application/json') || ct.includes('application/problem+json')) {
            try {
                const j = await res.json();
                message =
                    j?.message ||
                    j?.detail ||
                    j?.error ||
                    j?.title ||
                    j?.errors?.[0]?.defaultMessage ||
                    JSON.stringify(j);
            } catch {
            }
        }
        if (!message || message === res.statusText) {
            try {
                const t = await res.text();
                if (t) message = t;
            } catch {}
        }
        throw new Error(message || 'Request failed');
    }

    return (await res.json()) as T;
}

export function decodeJwt<T = unknown>(token: string): T | null {
    try {
        const [, payload] = token.split('.');
        const json = atob(payload.replace(/-/g, '+').replace(/_/g, '/'));
        return JSON.parse(json) as T;
    } catch {
        return null;
    }
}

export async function loginRequest(login: string, password: string) {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ login, password }),
    });
    if (!res.ok) throw new Error('Unauthorized');
    return res.json() as Promise<{ token: string }>;
}
