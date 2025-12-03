import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {roomsApi} from '@/api/rooms';
import type {RoomResponse} from '@/types/rooms';

export const RoomsListPage: React.FC = () => {
    const [items, setItems] = useState<RoomResponse[]>([]);
    const [loading, setLoading] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await roomsApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    return (
        <Page title="Аудитории">
            {loading && <p>Загрузка…</p>}
            {!loading && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Код</th>
                        <th align="left">Корпус</th>
                        <th align="left">Номер</th>
                        <th align="left">Вместимость</th>
                        <th align="left">Статус</th>
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(r => (
                        <tr key={r.id}>
                            <td>{r.roomCode}</td>
                            <td>{r.building}</td>
                            <td>{r.number}</td>
                            <td>{r.capacity}</td>
                            <td>{r.status}</td>
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={5}>Нет аудиторий</td>
                        </tr>
                    )}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
