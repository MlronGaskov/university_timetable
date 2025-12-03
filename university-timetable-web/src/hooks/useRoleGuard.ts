import {useAuth} from '@/providers/AuthProvider';

export const useRoleGuard = () => {
    const {role} = useAuth();
    return {
        role,
        isAdmin: role === 'ADMIN',
        isTeacher: role === 'TEACHER',
        isStudent: role === 'STUDENT',
    };
};
