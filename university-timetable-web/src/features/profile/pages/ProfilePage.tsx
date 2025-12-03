import React from 'react';
import {Page} from '@/components/layout/Page';
import {useAuth} from '@/providers/AuthProvider';
import {useRoleGuard} from '@/hooks/useRoleGuard';

export const ProfilePage: React.FC = () => {
    const {userId, teacherId, studentId} = useAuth();
    const {role, isAdmin, isTeacher, isStudent} = useRoleGuard();

    return (
        <Page title="Профиль">
            <div style={{display: 'flex', flexDirection: 'column', gap: 16}}>
                <section>
                    <h2 style={{marginTop: 0}}>Основная информация</h2>
                    <p>Роль: <b>{role ?? '—'}</b></p>
                    <p>User ID: <code>{userId ?? '—'}</code></p>
                    <p>Teacher ID: <code>{teacherId ?? '—'}</code></p>
                    <p>Student ID: <code>{studentId ?? '—'}</code></p>
                </section>

                {isTeacher && (
                    <section>
                        <h2>Настройки преподавателя</h2>
                        <p>Здесь будет форма редактирования желаемых часов работы (working hours).</p>
                    </section>
                )}

                {isStudent && (
                    <section>
                        <h2>Информация студента</h2>
                        <p>Здесь можно будет показать связанные группы и курсы.</p>
                    </section>
                )}

                {isAdmin && (
                    <section>
                        <h2>Админский профиль</h2>
                        <p>Техническая информация, ссылки на административные страницы и т.д.</p>
                    </section>
                )}
            </div>
        </Page>
    );
};
