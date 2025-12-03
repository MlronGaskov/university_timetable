import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {teachersApi} from '@/api/teachers';
import type {TeacherResponse} from '@/types/teachers';

export const TeachersListPage: React.FC = () => {
    const [items, setItems] = useState<TeacherResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await teachersApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Преподаватели">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">ID</th>
                        <th align="left">ФИО</th>
                        <th align="left">Статус</th>
                        <th align="left">Часы работы</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(t => (
                        <tr key={t.id}>
                            <td>{t.teacherId}</td>
                            <td>{t.fullName}</td>
                            <td>{t.status}</td>
                            <td>{t.preferredWorkingHours && t.preferredWorkingHours.length > 0 ? 'Заданы' : '—'}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={4}>Нет преподавателей</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
