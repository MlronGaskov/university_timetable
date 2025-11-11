export type Role = 'ADMIN' | 'TEACHER' | 'STUDENT';
export type Status = 'ACTIVE' | 'INACTIVE';

export interface UserDto {
    id: string;
    login: string;
    email: string;
    role: Role;
    status: Status;
    createdAt: string;
    updatedAt: string;
}
