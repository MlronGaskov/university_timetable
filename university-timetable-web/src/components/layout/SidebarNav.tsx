import React from 'react';
import {NavLink} from 'react-router-dom';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import styles from './SidebarNav.module.css';

export const SidebarNav: React.FC = () => {
    const {isAdmin, isTeacher, isStudent} = useRoleGuard();

    const link = ({isActive}: {isActive: boolean}) => isActive ? styles.activeLink : styles.link;

    return (
        <aside className={styles.sidebar}>
            <div className={styles.logo}>UT</div>
            <nav className={styles.nav}>
                <NavLink to="/profile" className={link}>Профиль</NavLink>

                <div className={styles.sectionTitle}>Учебный процесс</div>
                <NavLink to="/courses" className={link}>Курсы</NavLink>
                <NavLink to="/groups" className={link}>Группы</NavLink>
                <NavLink to="/rooms" className={link}>Аудитории</NavLink>
                <NavLink to="/semesters" className={link}>Семестры</NavLink>
                <NavLink to="/teachers" className={link}>Преподаватели</NavLink>
                <NavLink to="/students" className={link}>Студенты</NavLink>

                {isAdmin && (
                    <>
                        <div className={styles.sectionTitle}>Администрирование</div>
                        <NavLink to="/admin/users" className={link}>Пользователи</NavLink>
                        <NavLink to="/policies" className={link}>Политики</NavLink>
                    </>
                )}
            </nav>
        </aside>
    );
};
