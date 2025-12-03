import type {ReactNode} from 'react';
import {Navigate} from 'react-router-dom';
import {useAuth} from '@/hooks/useAuth';
import type {Role} from '@/types/auth';

interface Props {
    children: ReactNode;
    roles: Role[];
}

export const RequireRole = ({children, roles}: Props) => {
    const {role} = useAuth();

    if (!role || !roles.includes(role)) {
        return <Navigate to="/" replace/>;
    }

    return <>{children}</>;
};
