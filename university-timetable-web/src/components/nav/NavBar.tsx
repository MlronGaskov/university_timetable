import type {FC} from 'react';
import {Link, NavLink} from 'react-router-dom';
import {useAuth} from '@/hooks/useAuth';
import './NavBar.css';

export const NavBar: FC = () => {
    const {isAuthenticated, role, logout} = useAuth();

    const isAdmin = role === 'ADMIN';

    return (
        <header className="NavBar">
            <div className="NavBar-left">
                <Link to="/" className="NavBar-logo">
                    NSU · Расписание
                </Link>

                {isAuthenticated && (
                    <nav className="NavBar-nav">
                        <NavLink to="/" end className="NavBar-link">
                            Главная
                        </NavLink>

                        <NavLink to="/timetable" className="NavBar-link">
                            Расписание
                        </NavLink>

                        <NavLink to="/profile" className="NavBar-link">
                            Профиль
                        </NavLink>

                        {!isAdmin && (
                            <>
                                <NavLink to="/teachers" className="NavBar-link">
                                    Преподаватели
                                </NavLink>
                                <NavLink to="/students" className="NavBar-link">
                                    Студенты
                                </NavLink>
                                <NavLink to="/groups" className="NavBar-link">
                                    Группы
                                </NavLink>
                                <NavLink to="/rooms" className="NavBar-link">
                                    Аудитории
                                </NavLink>
                            </>
                        )}

                        {isAdmin && (
                            <>
                                <NavLink to="/admin/users" className="NavBar-link">
                                    Пользователи
                                </NavLink>
                                <NavLink to="/admin/teachers" className="NavBar-link">
                                    Преподаватели
                                </NavLink>
                                <NavLink to="/admin/students" className="NavBar-link">
                                    Студенты
                                </NavLink>
                                <NavLink to="/admin/groups" className="NavBar-link">
                                    Группы
                                </NavLink>
                                <NavLink to="/admin/rooms" className="NavBar-link">
                                    Аудитории
                                </NavLink>
                                <NavLink to="/admin/policies" className="NavBar-link">
                                    Политики
                                </NavLink>
                            </>
                        )}
                    </nav>
                )}
            </div>

            <div className="NavBar-right">
                {!isAuthenticated && (
                    <Link to="/login" className="NavBar-button primary">
                        Войти
                    </Link>
                )}

                {isAuthenticated && (
                    <>
                        {role && (
                            <span className="NavBar-role">
                                Роль: <strong>{role}</strong>
                            </span>
                        )}
                        <button
                            className="NavBar-button"
                            type="button"
                            onClick={logout}
                        >
                            Выйти
                        </button>
                    </>
                )}
            </div>
        </header>
    );
};
