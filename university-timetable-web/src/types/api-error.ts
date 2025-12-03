export interface ApiFieldError {
    field: string | null;
    message: string;
}

export interface ApiErrorResponse {
    timestamp: string;
    status: number;
    error: string;
    message: string;
    path: string;
    fieldErrors: ApiFieldError[];
}

export interface HttpError extends Error {
    status?: number;
    body?: unknown;
}

export function isHttpError(err: unknown): err is HttpError {
    return err instanceof Error;
}

export function parseApiErrorBody(body: unknown): ApiErrorResponse | null {
    if (!body || typeof body !== 'object') return null;
    const anyBody = body as any;
    if (typeof anyBody.status === 'number' && typeof anyBody.error === 'string') {
        return {
            timestamp: String(anyBody.timestamp ?? ''),
            status: anyBody.status,
            error: anyBody.error,
            message: String(anyBody.message ?? ''),
            path: String(anyBody.path ?? ''),
            fieldErrors: Array.isArray(anyBody.fieldErrors)
                ? anyBody.fieldErrors.map((f: any) => ({
                    field: f.field ?? null,
                    message: String(f.message ?? ''),
                }))
                : [],
        };
    }
    return null;
}
