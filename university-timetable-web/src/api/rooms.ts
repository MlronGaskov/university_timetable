import {http} from './http';
import type {
    RoomResponse,
    CreateRoomRequest,
    UpdateRoomRequest,
} from '@/types/room';
import type {UUID} from '@/types/common';

export const roomsApi = {
    getAll() {
        return http<RoomResponse[]>('/api/rooms');
    },

    getOne(id: UUID) {
        return http<RoomResponse>(`/api/rooms/${id}`);
    },

    create(body: CreateRoomRequest) {
        return http<RoomResponse>('/api/rooms', {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateRoomRequest) {
        return http<RoomResponse>(`/api/rooms/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<RoomResponse>(`/api/rooms/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<RoomResponse>(`/api/rooms/${id}/activate`, {
            method: 'POST',
        });
    },
};
