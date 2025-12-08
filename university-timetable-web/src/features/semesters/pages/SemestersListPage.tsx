import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {semestersApi} from '@/api/semesters';
import type {SemesterResponse} from '@/types/semesters';
import {formatDate} from '@/utils/formatters';
import {Link} from "react-router-dom";

export const SemestersListPage: React.FC = () => {
    const [items, setItems] = useState<SemesterResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await semestersApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Семестры">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Код</th>
                        <th align="left">Начало</th>
                        <th align="left">Конец</th>
                        <th align="left">Статус</th>
                        <th align="left">Расписания</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(s => (
                        <tr key={s.id}>
                            <td>{s.code}</td>
                            <td>{formatDate(s.startAt)}</td>
                            <td>{formatDate(s.endAt)}</td>
                            <td>{s.status}</td>
                            <td>
                                <Link to={`/semesters/${s.id}/schedules`}>Открыть</Link>
                            </td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={5}>Нет семестров</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
