import React from 'react';
import {useAuth} from '@/providers/AuthProvider';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import styles from './TopBar.module.css';

export const TopBar: React.FC = () => {
    const {logout} = useAuth();
    const {role} = useRoleGuard();

    return (
        <header className={styles.root}>
            <div className={styles.left}>
                <span className={styles.appTitle}>University Timetable</span>
                {role && <span className={styles.roleBadge}>{role}</span>}
            </div>
            <div className={styles.right}>
                <button className={styles.logoutBtn} onClick={logout}>
                    Выйти
                </button>
            </div>
        </header>
    );
};
