import {http} from './http';
import type {NotificationEntryDto} from '@/types/notifications';

export const notificationsApi = {
    getAll() {
        return http<NotificationEntryDto[]>('/api/notifications');
    },
};
