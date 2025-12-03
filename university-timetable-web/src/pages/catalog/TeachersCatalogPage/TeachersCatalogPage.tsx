import React, {useEffect, useState} from "react";
import {teachersApi} from "@/api/teachers";
import type {TeacherResponse} from "@/types/teacher";
import "../CatalogPage.css";

export const TeachersCatalogPage: React.FC = () => {
    const [items, setItems] = useState<TeacherResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const load = async () => {
            try {
                setError(null);
                const data = await teachersApi.getAll();
                setItems(data);
            } catch (e) {
                setError("Не удалось загрузить преподавателей");
            } finally {
                setLoading(false);
            }
        };
        void load();
    }, []);

    const formatIntervals = (t: TeacherResponse) =>
        !t.preferredWorkingHours || t.preferredWorkingHours.length === 0
            ? "—"
            : t.preferredWorkingHours
                .map(h => `${h.day} ${h.startTime}–${h.endTime}`)
                .join(", ");

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Преподаватели</h1>
                        <p className="Page-headerSubtitle">
                            Справочник преподавателей (только чтение). Видны UID, статус и рабочие интервалы.
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
                                <th>ФИО</th>
                                <th>Статус</th>
                                <th>Рабочие интервалы</th>
                                <th>Создан</th>
                                <th>Обновлён</th>
                            </tr>
                            </thead>
                            <tbody>
                            {items.map(t => (
                                <tr key={t.id}>
                                    <td>
                                        <code>{t.id}</code>
                                    </td>
                                    <td>{t.fullName}</td>
                                    <td>{t.status}</td>
                                    <td>{formatIntervals(t)}</td>
                                    <td>{new Date(t.createdAt).toLocaleString()}</td>
                                    <td>{new Date(t.updatedAt).toLocaleString()}</td>
                                </tr>
                            ))}
                            {items.length === 0 && (
                                <tr>
                                    <td colSpan={6} className="CatalogPage-empty">
                                        Преподаватели не найдены
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
