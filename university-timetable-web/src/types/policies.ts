import type {UUID} from './common';

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
    createdAt: string;
    updatedAt: string;
}
