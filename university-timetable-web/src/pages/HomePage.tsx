import React from 'react';
import Button from '../components/ui/Button';
import {useAuth} from '@hooks/useAuth';
import {useNavigate} from 'react-router-dom';

export default function HomePage() {
    const {isAuthed, isAdmin, role, logout} = useAuth();
    const nav = useNavigate();

    return (
        <div className="page">
            <header className="hdr">
                <div className="page-header-title">Home</div>
                <div style={{display: 'flex', gap: 8}}>
                    {isAuthed ? (
                        <Button onClick={logout}>Выйти</Button>
                    ) : (
                        <Button onClick={() => nav('/login')} className="primary">Войти</Button>
                    )}
                </div>
            </header>

            <main className="container page-main">
                <div className="card" style={{padding: 16}}>
                    <h2 style={{marginTop: 0}}>Добро пожаловать!</h2>
                    {!isAuthed && <p>Пожалуйста, войдите, чтобы продолжить.</p>}

                    {isAuthed && (
                        <>
                            <p>Вы вошли как: <b>{role}</b>.</p>
                            {isAdmin ? (
                                <div style={{display: 'flex', gap: 8}}>
                                    <Button className="primary" onClick={() => nav('/users')}>Перейти к
                                        пользователям</Button>
                                </div>
                            ) : (
                                <p>У вас нет доступа к разделу пользователей.</p>
                            )}
                        </>
                    )}
                </div>
            </main>
        </div>
    );
}
