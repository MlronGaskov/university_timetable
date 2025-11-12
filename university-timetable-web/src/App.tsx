import React from 'react';
import {BrowserRouter, Routes, Route, Navigate} from 'react-router-dom';
import HomePage from './pages/HomePage';
import UsersPage from './pages/UsersPage';
import LoginPage from './pages/LoginPage';
import {useAuth} from '@hooks/useAuth';

function RequireAuth({children}: { children: React.ReactElement }) {
    const {isAuthed} = useAuth();
    if (!isAuthed) return <Navigate to="/login" replace/>;
    return children;
}

function RequireAdmin({children}: { children: React.ReactElement }) {
    const {isAuthed, isAdmin} = useAuth();
    if (!isAuthed) return <Navigate to="/login" replace/>;
    if (!isAdmin) return <Navigate to="/" replace/>;
    return children;
}

export default function App() {
    return (
        <BrowserRouter>
            <Routes>
                <Route path="/" element={<HomePage/>}/>
                <Route
                    path="/users"
                    element={
                        <RequireAdmin>
                            <UsersPage/>
                        </RequireAdmin>
                    }
                />
                <Route path="/login" element={<LoginPage/>}/>
                <Route path="*" element={<Navigate to="/" replace/>}/>
            </Routes>
        </BrowserRouter>
    );
}
