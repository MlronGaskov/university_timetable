import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {studentsApi} from '@/api/students';
import type {StudentResponse} from '@/types/students';

export const StudentsListPage: React.FC = () => {
    const [items, setItems] = useState<StudentResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await studentsApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Студенты">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">ID</th>
                        <th align="left">ФИО</th>
                        <th align="left">Статус</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(s => (
                        <tr key={s.id}>
                            <td>{s.studentId}</td>
                            <td>{s.fullName}</td>
                            <td>{s.status}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={3}>Нет студентов</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
