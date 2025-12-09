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
    groupCodes: string[];
}

export interface UpdateCourseRequest {
    code?: string;
    title?: string;
    teacherId?: string;
    plannedHours?: number;
    equipmentRequirements?: CourseEquipmentItemDto[] | null;
    groupCodes?: string[] | null;
}

export interface CourseResponse {
    id: UUID;
    code: string;
    title: string;
    status: Status;
    plannedHours: number;
    requiredRoomCapacity: number;
    teacherId: string;
    groupCodes: string[];
    equipmentRequirements: CourseEquipmentItemDto[];
    createdAt: string;
    updatedAt: string;
}
