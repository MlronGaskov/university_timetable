import React from 'react';
import {Navigate, Outlet} from 'react-router-dom';
import {useRoleGuard} from '@/hooks/useRoleGuard';

interface RoleRouteProps {
    allowed: Array<'ADMIN' | 'TEACHER' | 'STUDENT'>;
}

export const RoleRoute: React.FC<RoleRouteProps> = ({allowed}) => {
    const {role} = useRoleGuard();
    if (!role || !allowed.includes(role)) {
        return <Navigate to="/profile" replace/>;
    }
    return <Outlet/>;
};
