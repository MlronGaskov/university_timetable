import type {UUID, Instant, Status} from './common';

export interface RoomItemDto {
    name: string;
    quantity: number;
}

export interface CreateRoomRequest {
    building: string;
    number: string;
    capacity: number;
    items: RoomItemDto[];
}

export interface UpdateRoomRequest {
    building?: string;
    number?: string;
    capacity?: number;
    items?: RoomItemDto[];
}

export interface RoomResponse {
    id: UUID;
    building: string;
    number: string;
    capacity: number;
    status: Status;
    createdAt: Instant;
    updatedAt: Instant;
    items: RoomItemDto[];
}
