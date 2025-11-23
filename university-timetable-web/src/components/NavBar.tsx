import React from 'react';
import Button from './ui/Button';
import {useNavigate} from 'react-router-dom';
import {useAuth} from '@hooks/useAuth';

export default function NavBar() {
    const nav = useNavigate();
    const {isAuthed, isAdmin, logout} = useAuth();

    if (!isAuthed) {
        return (
            <div style={{display: 'flex', gap: 8}}>
                <Button onClick={() => nav('/login')} className="primary">
                    Войти
                </Button>
            </div>
        );
    }

    return (
        <div style={{display: 'flex', flexWrap: 'wrap', gap: 8, justifyContent: 'flex-end'}}>
            <Button onClick={() => nav('/')}>Главная</Button>
            <Button onClick={() => nav('/rooms')}>Аудитории</Button>
            <Button onClick={() => nav('/students')}>Студенты</Button>
            <Button onClick={() => nav('/groups')}>Группы</Button>
            <Button onClick={() => nav('/teachers')}>Преподаватели</Button>
            {isAdmin && (
                <Button onClick={() => nav('/users')}>Пользователи</Button>
            )}
            <Button onClick={logout}>Выйти</Button>
        </div>
    );
}
