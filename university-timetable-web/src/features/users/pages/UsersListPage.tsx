import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {usersApi} from '@/api/users';
import type {UserResponse} from '@/types/auth';

export const UsersListPage: React.FC = () => {
    const [items, setItems] = useState<UserResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await usersApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Пользователи">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Логин</th>
                        <th align="left">Email</th>
                        <th align="left">Роль</th>
                        <th align="left">Статус</th>
                        <th align="left">Преподаватель</th>
                        <th align="left">Студент</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(u => (
                        <tr key={u.id}>
                            <td>{u.login}</td>
                            <td>{u.email}</td>
                            <td>{u.role}</td>
                            <td>{u.status}</td>
                            <td>{u.teacherName ?? '—'}</td>
                            <td>{u.studentName ?? '—'}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={6}>Нет пользователей</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
