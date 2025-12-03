import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {policiesApi} from '@/api/policies';
import type {PolicyResponse} from '@/types/policies';

export const PoliciesListPage: React.FC = () => {
    const [items, setItems] = useState<PolicyResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await policiesApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Политики">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Название</th>
                        <th align="left">ID</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(p => (
                        <tr key={p.id}>
                            <td>{p.name}</td>
                            <td>{p.id}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={2}>Нет политик</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
