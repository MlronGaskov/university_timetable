import React from 'react';
import {BrowserRouter, Routes, Route, Navigate} from 'react-router-dom';
import HomePage from './pages/HomePage';
import UsersPage from './pages/UsersPage';
import LoginPage from './pages/LoginPage';
import RoomsPage from './pages/RoomsPage';
import StudentsPage from './pages/StudentsPage';
import {useAuth} from '@hooks/useAuth';
import TeachersPage from './pages/TeachersPage';
import GroupsPage from './pages/GroupsPage';

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

                <Route
                    path="/rooms"
                    element={
                        <RequireAuth>
                            <RoomsPage/>
                        </RequireAuth>
                    }
                />

                <Route
                    path="/students"
                    element={
                        <RequireAuth>
                            <StudentsPage/>
                        </RequireAuth>
                    }
                />
                <Route
                    path="/groups"
                    element={
                        <RequireAuth>
                            <GroupsPage/>
                        </RequireAuth>
                    }
                />
                <Route
                    path="/teachers"
                    element={
                        <RequireAuth>
                            <TeachersPage/>
                        </RequireAuth>
                    }
                />

                <Route path="/login" element={<LoginPage/>}/>
                <Route path="*" element={<Navigate to="/" replace/>}/>
            </Routes>
        </BrowserRouter>
    );
}
