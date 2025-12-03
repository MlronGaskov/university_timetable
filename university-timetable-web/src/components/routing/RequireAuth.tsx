import type {ReactNode} from 'react';
import {Navigate, useLocation} from 'react-router-dom';
import {useAuth} from '@/hooks/useAuth';

interface Props {
    children: ReactNode;
}

export const RequireAuth = ({children}: Props) => {
    const {isAuthenticated} = useAuth();
    const location = useLocation();

    if (!isAuthenticated) {
        return <Navigate to="/login" replace state={{from: location}}/>;
    }

    return <>{children}</>;
};
