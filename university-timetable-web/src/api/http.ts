const API_BASE_URL =
    import.meta.env.VITE_API_BASE ?? 'http://localhost:8080';

let authToken: string | null = null;

export type ApiError = Error & {
    status?: number;
    body?: unknown;
};

export const setAuthToken = (token: string | null) => {
    authToken = token;
};

export const ifMatchHeaders = (version: number): HeadersInit => ({
    'If-Match': String(version),
});

export async function http<T>(
    path: string,
    options: RequestInit = {}
): Promise<T> {
    const url = API_BASE_URL + path;

    const headers = new Headers(options.headers);

    if (!headers.has('Content-Type')) {
        headers.set('Content-Type', 'application/json');
    }

    if (authToken) {
        headers.set('Authorization', `Bearer ${authToken}`);
    }

    const response = await fetch(url, {
        ...options,
        headers,
    });

    if (!response.ok) {
        let errorBody: unknown = null;

        try {
            errorBody = await response.json();
        } catch {
            try {
                errorBody = await response.text();
            } catch {
                errorBody = null;
            }
        }

        const error = new Error(
            `HTTP ${response.status} ${response.statusText} for ${path}`
        ) as ApiError;

        error.status = response.status;
        error.body = errorBody;

        throw error;
    }

    if (response.status === 204) {
        return undefined as T;
    }

    return (await response.json()) as T;
}