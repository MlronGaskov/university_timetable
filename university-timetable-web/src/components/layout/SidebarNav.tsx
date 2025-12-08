import React from 'react';
import {NavLink} from 'react-router-dom';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import styles from './SidebarNav.module.css';

export const SidebarNav: React.FC = () => {
    const {isAdmin, isTeacher, isStudent} = useRoleGuard();

    return (
        <aside className={styles.sidebar}>
            <div className={styles.logo}>UT</div>
            <nav className={styles.nav}>
                <NavLink to="/profile" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Профиль
                </NavLink>

                <NavLink to="/courses" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Курсы
                </NavLink>
                <NavLink to="/groups" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Группы
                </NavLink>
                <NavLink to="/rooms" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Аудитории
                </NavLink>
                <NavLink to="/semesters" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Семестры
                </NavLink>
                <NavLink to="/teachers" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Преподаватели
                </NavLink>
                <NavLink to="/students" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                    Студенты
                </NavLink>

                {isAdmin && (
                    <>
                        <NavLink to="/admin/users"
                                 className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                            Пользователи
                        </NavLink>
                        <NavLink to="/policies" className={({isActive}) => isActive ? styles.activeLink : styles.link}>
                            Политики
                        </NavLink>
                    </>
                )}
            </nav>
        </aside>
    );
};
