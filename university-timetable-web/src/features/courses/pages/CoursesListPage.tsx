import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {coursesApi} from '@/api/courses';
import type {CourseResponse} from '@/types/courses';
import {formatInstant} from '@/utils/formatters';

export const CoursesListPage: React.FC = () => {
    const [items, setItems] = useState<CourseResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await coursesApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Курсы">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Код</th>
                        <th align="left">Название</th>
                        <th align="left">Преподаватель ID</th>
                        <th align="left">Часы</th>
                        <th align="left">Статус</th>
                        <th align="left">Обновлено</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(c => (
                        <tr key={c.id}>
                            <td>{c.code}</td>
                            <td>{c.title}</td>
                            <td>{c.teacherId}</td>
                            <td>{c.plannedHours}</td>
                            <td>{c.status}</td>
                            <td>{formatInstant(c.updatedAt)}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={6}>Нет курсов</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
