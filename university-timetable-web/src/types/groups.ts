import type {Status, UUID} from './common';

export interface CreateGroupRequest {
    name: string;
    code: string;
    size: number;
}

export interface UpdateGroupRequest {
    name?: string;
    code?: string;
    size?: number;
}

export interface GroupResponse {
    id: UUID;
    name: string;
    code: string;
    size: number;
    status: Status;
    version: number;
    createdAt: string;
    updatedAt: string;
    studentIds: string[];
}
