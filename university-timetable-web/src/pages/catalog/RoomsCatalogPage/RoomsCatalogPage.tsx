import React, {useEffect, useState} from "react";
import {roomsApi} from "@/api/rooms";
import type {RoomResponse} from "@/types/room";
import "../CatalogPage.css";

export const RoomsCatalogPage: React.FC = () => {
    const [items, setItems] = useState<RoomResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const load = async () => {
            try {
                setError(null);
                const data = await roomsApi.getAll();
                setItems(data);
            } catch (e) {
                setError("Не удалось загрузить аудитории");
            } finally {
                setLoading(false);
            }
        };
        void load();
    }, []);

    const formatItems = (r: RoomResponse) =>
        !r.items || r.items.length === 0
            ? "—"
            : r.items.map(it => `${it.name} (${it.quantity})`).join(", ");

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Аудитории</h1>
                        <p className="Page-headerSubtitle">
                            Справочник аудиторий (только чтение). Видны UID, корпус, номер, вместимость, статус и
                            оборудование.
                        </p>
                    </div>
                </div>

                {loading && <div>Загрузка...</div>}
                {error && <div className="CatalogPage-error">{error}</div>}

                {!loading && !error && (
                    <div className="CatalogPage-tableWrapper">
                        <table className="CatalogPage-table">
                            <thead>
                            <tr>
                                <th>UID</th>
                                <th>Корпус</th>
                                <th>Номер</th>
                                <th>Вместимость</th>
                                <th>Статус</th>
                                <th>Оборудование</th>
                                <th>Создана</th>
                                <th>Обновлена</th>
                            </tr>
                            </thead>
                            <tbody>
                            {items.map(r => (
                                <tr key={r.id}>
                                    <td>
                                        <code>{r.id}</code>
                                    </td>
                                    <td>{r.building}</td>
                                    <td>{r.number}</td>
                                    <td>{r.capacity}</td>
                                    <td>{r.status}</td>
                                    <td>{formatItems(r)}</td>
                                    <td>{new Date(r.createdAt).toLocaleString()}</td>
                                    <td>{new Date(r.updatedAt).toLocaleString()}</td>
                                </tr>
                            ))}
                            {items.length === 0 && (
                                <tr>
                                    <td colSpan={8} className="CatalogPage-empty">
                                        Аудитории не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}
            </div>
        </div>
    );
};
