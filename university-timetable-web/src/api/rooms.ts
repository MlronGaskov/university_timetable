import {RoomDto} from '@/types';
import {api} from './client';

export type RoomEquipmentItemReq = { name: string; quantity: number };

export type CreateRoomReq = {
    building: string;
    number: string;
    capacity: number;
    equipment?: RoomEquipmentItemReq[];
};

export type UpdateRoomReq = {
    building?: string;
    number?: string;
    capacity?: number;
    equipment?: RoomEquipmentItemReq[];
};

export const RoomsApi = {
    list: () => api<RoomDto[]>('/api/rooms'),
    get: (id: string) => api<RoomDto>(`/api/rooms/${id}`),

    create: (body: CreateRoomReq) =>
        api<RoomDto>('/api/rooms', {
            method: 'POST',
            body: JSON.stringify(body),
        }),

    update: (id: string, body: UpdateRoomReq) =>
        api<RoomDto>(`/api/rooms/${id}`, {
            method: 'PUT',
            body: JSON.stringify(body),
        }),

    archive: (id: string) =>
        api<RoomDto>(`/api/rooms/${id}/archive`, {method: 'POST'}),
};
