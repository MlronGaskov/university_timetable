import type {Status, UUID} from './common';

export interface CourseEquipmentItemDto {
    name: string;
    quantity: number;
}

export interface CreateCourseRequest {
    code: string;
    title: string;
    teacherId: string;
    plannedHours: number;
    equipmentRequirements: CourseEquipmentItemDto[];
    groupIds: UUID[];
}

export interface UpdateCourseRequest {
    code?: string;
    title?: string;
    teacherId?: string;
    plannedHours?: number;
    equipmentRequirements?: CourseEquipmentItemDto[] | null;
    groupIds?: UUID[] | null;
}

export interface CourseResponse {
    id: UUID;
    code: string;
    title: string;
    status: Status;
    plannedHours: number;
    requiredRoomCapacity: number;
    teacherId: string;
    groupIds: UUID[];
    equipmentRequirements: CourseEquipmentItemDto[];
    createdAt: string;
    updatedAt: string;
}
