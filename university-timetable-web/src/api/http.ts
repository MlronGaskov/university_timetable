const API_BASE_URL =
    import.meta.env.VITE_API_BASE ?? 'http://localhost:8080';

let authToken: string | null = null;

export const setAuthToken = (token: string | null) => {
    authToken = token;
};

export async function http<T>(
    path: string,
    options: RequestInit = {}
): Promise<T> {
    const url = API_BASE_URL + path;

    const headers: HeadersInit = {
        'Content-Type': 'application/json',
        ...(options.headers || {}),
    };

    if (authToken) {
        (headers as any).Authorization = `Bearer ${authToken}`;
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
        ) as Error & { status?: number; body?: unknown };
        error.status = response.status;
        error.body = errorBody;
        throw error;
    }

    if (response.status === 204) {
        return undefined as T;
    }

    return (await response.json()) as T;
}
