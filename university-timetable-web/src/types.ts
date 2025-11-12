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
