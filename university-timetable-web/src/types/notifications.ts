export interface SlotChangeDto {
    action: 'added' | 'removed';
    description: string;
}

export interface UserNotificationDto {
    userType: 'teacher' | 'student';
    fullName: string;
    userId: string;
    email: string;
    changes: SlotChangeDto[];
}

export interface NotificationEntryDto {
    timestamp: string;
    semesterCode: string | null;
    reason: string | null;
    users: UserNotificationDto[];
}
