import type {UUID, Instant, Status} from './common';

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
    createdAt: Instant;
    updatedAt: Instant;
    studentIds: UUID[];
}
