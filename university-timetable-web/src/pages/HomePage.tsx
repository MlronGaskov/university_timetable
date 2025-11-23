import React from 'react';
import {useAuth} from '@hooks/useAuth';
import NavBar from '../components/NavBar';

export default function HomePage() {
    const {isAuthed, isAdmin, role} = useAuth();

    return (
        <div className="page">
            <header className="hdr">
                <div className="page-header-title">Главная</div>
                <NavBar/>
            </header>

            <main className="container page-main">
                <div className="card" style={{padding: 16}}>
                    <h2 style={{marginTop: 0}}>Добро пожаловать!</h2>
                    {!isAuthed && <p>Пожалуйста, войдите, чтобы продолжить.</p>}
                    {isAuthed && (
                        <p>
                            Вы вошли как: <b>{role}</b>{isAdmin ? ' (администратор)' : ''}.
                        </p>
                    )}
                </div>
            </main>
        </div>
    );
}
