import React, {JSX} from 'react';
import {BrowserRouter, Route, Routes, Navigate} from 'react-router-dom';
import LoginPage from './pages/LoginPage';
import UsersPage from './pages/UsersPage';

function RequireAuth({children}: { children: JSX.Element }) {
    const token = localStorage.getItem('jwt');
    if (!token) return <Navigate to="/login" replace/>;
    return children;
}

export default function App() {
    return (
        <BrowserRouter>
            <Routes>
                <Route path="/login" element={<LoginPage/>}/>
                <Route path="/" element={<RequireAuth><UsersPage/></RequireAuth>}/>
            </Routes>
        </BrowserRouter>
    );
}
