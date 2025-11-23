export type UserDto = {
    id: string;
    login: string;
    email: string;
    status: 'ACTIVE' | 'INACTIVE';
    role: 'ADMIN' | 'TEACHER' | 'STUDENT';
    createdAt: string;
    updatedAt: string;
};

export type Role = UserDto['role'];
export type Status = UserDto['status'];

export type CreateUserResult = {
    user: UserDto;
    tempPassword?: string | null;
};

export type TokenPayload = {
    sub: string;
    role: Role;
    exp: number;
    iat: number;
};

/* ---------- Groups / Subgroups ---------- */

export type SubgroupDto = {
    id: string;
    name: string;
    size: number;
    status: Status;
};

export type GroupDto = {
    id: string;
    name: string;
    code: string;
    size: number;
    status: Status;
    createdAt: string;
    updatedAt: string;
    subgroups: SubgroupDto[];
};

/* ---------- Students ---------- */

export type StudentDto = {
    id: string;
    fullName: string;
    studentId: string;
    groupId: string | null;
    groupName: string | null;
    status: Status;
    createdAt: string;
    updatedAt: string;
};

/* ---------- Teachers ---------- */

export type TeacherDto = {
    id: string;
    fullName: string;
    status: Status;
    createdAt: string;
    updatedAt: string;
};

/* ---------- Rooms ---------- */

export type RoomEquipmentDto = {
    id: string;
    name: string;
    quantity: number;
};

export type RoomDto = {
    id: string;
    building: string;
    number: string;
    capacity: number;
    status: Status;
    createdAt: string;
    updatedAt: string;
    equipment: RoomEquipmentDto[];
};
