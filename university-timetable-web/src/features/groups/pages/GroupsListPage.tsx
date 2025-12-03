import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {groupsApi} from '@/api/groups';
import type {GroupResponse} from '@/types/groups';

export const GroupsListPage: React.FC = () => {
    const [items, setItems] = useState<GroupResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await groupsApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Группы">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Код</th>
                        <th align="left">Название</th>
                        <th align="left">Размер</th>
                        <th align="left">Статус</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(g => (
                        <tr key={g.id}>
                            <td>{g.code}</td>
                            <td>{g.name}</td>
                            <td>{g.size}</td>
                            <td>{g.status}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={4}>Нет групп</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
