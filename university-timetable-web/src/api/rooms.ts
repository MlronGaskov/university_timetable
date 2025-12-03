import {http} from './http';
import type {CreateRoomRequest, RoomResponse, UpdateRoomRequest,} from '@/types/rooms';
import type {UUID} from '@/types/common';

const BASE_PATH = '/api/rooms';

export const roomsApi = {
    getAll() {
        return http<RoomResponse[]>(BASE_PATH);
    },

    getById(id: UUID) {
        return http<RoomResponse>(`${BASE_PATH}/${id}`);
    },

    create(body: CreateRoomRequest) {
        return http<RoomResponse>(BASE_PATH, {
            method: 'POST',
            body: JSON.stringify(body),
        });
    },

    update(id: UUID, body: UpdateRoomRequest) {
        return http<RoomResponse>(`${BASE_PATH}/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        });
    },

    archive(id: UUID) {
        return http<RoomResponse>(`${BASE_PATH}/${id}/archive`, {
            method: 'POST',
        });
    },

    activate(id: UUID) {
        return http<RoomResponse>(`${BASE_PATH}/${id}/activate`, {
            method: 'POST',
        });
    },
};
