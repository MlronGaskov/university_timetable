import type {UUID, Instant} from './common';

export interface CreatePolicyRequest {
    name: string;
    gridJson: string;
    breaksJson: string;
    limitsJson: string;
    travelMatrixJson: string;
    weightsJson: string;
}

export interface UpdatePolicyRequest {
    name?: string;
    gridJson?: string;
    breaksJson?: string;
    limitsJson?: string;
    travelMatrixJson?: string;
    weightsJson?: string;
}

export interface PolicyResponse {
    id: UUID;
    name: string;
    gridJson: string;
    breaksJson: string;
    limitsJson: string;
    travelMatrixJson: string;
    weightsJson: string;
    createdAt: Instant;
    updatedAt: Instant;
}
