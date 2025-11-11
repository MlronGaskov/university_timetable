const API_BASE = import.meta.env.VITE_API_BASE ?? 'http://localhost:8080';

function authHeaders() {
    const token = localStorage.getItem('jwt');
    const headers: Record<string, string> = {'Content-Type': 'application/json'};
    if (token) headers['Authorization'] = `Bearer ${token}`;
    return headers;
}

export async function api<T>(path: string, init?: RequestInit): Promise<T> {
    const res = await fetch(`${API_BASE}${path}`, {
        ...init,
        headers: {...authHeaders(), ...(init?.headers || {})},
    });
    if (res.status === 401) {
        localStorage.removeItem('jwt');
        location.reload();
    }
    if (!res.ok) {
        const text = await res.text();
        throw new Error(text || res.statusText);
    }
    return (await res.json()) as T;
}

export async function loginRequest(login: string, password: string) {
    const res = await fetch(`${API_BASE}/api/auth/login`, {
        method: 'POST',
        headers: {'Content-Type': 'application/json'},
        body: JSON.stringify({login, password})
    });
    if (!res.ok) throw new Error('Unauthorized');
    return res.json() as Promise<{ token: string }>;
}
