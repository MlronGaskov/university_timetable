import type {Status, UUID} from './common';

export interface RoomItemDto {
    name: string;
    quantity: number;
}

export interface CreateRoomRequest {
    roomCode: string;
    building: string;
    number: string;
    capacity: number;
    items?: RoomItemDto[];
}

export interface UpdateRoomRequest {
    roomCode?: string;
    building?: string;
    number?: string;
    capacity?: number;
    items?: RoomItemDto[];
}

export interface RoomResponse {
    id: UUID;
    roomCode: string;
    building: string;
    number: string;
    capacity: number;
    status: Status;
    createdAt: string;
    updatedAt: string;
    items: RoomItemDto[];
}
