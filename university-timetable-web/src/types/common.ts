export type UUID = string;

export type Status = 'ACTIVE' | 'INACTIVE';

export interface VersionedResource {
    version: number;
}

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