import React, {useEffect, useState} from "react";
import {studentsApi} from "@/api/students";
import type {StudentResponse} from "@/types/student";
import "../CatalogPage.css";

export const StudentsCatalogPage: React.FC = () => {
    const [items, setItems] = useState<StudentResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        const load = async () => {
            try {
                setError(null);
                const data = await studentsApi.getAll();
                setItems(data);
            } catch (e) {
                setError("Не удалось загрузить студентов");
            } finally {
                setLoading(false);
            }
        };
        void load();
    }, []);

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Студенты</h1>
                        <p className="Page-headerSubtitle">
                            Справочник студентов (только чтение). Видны UID, studentId и статус.
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
                                <th>studentId</th>
                                <th>Статус</th>
                                <th>Создан</th>
                                <th>Обновлён</th>
                            </tr>
                            </thead>
                            <tbody>
                            {items.map(s => (
                                <tr key={s.id}>
                                    <td>
                                        <code>{s.id}</code>
                                    </td>
                                    <td>{s.fullName}</td>
                                    <td>{s.studentId}</td>
                                    <td>{s.status}</td>
                                    <td>{new Date(s.createdAt).toLocaleString()}</td>
                                    <td>{new Date(s.updatedAt).toLocaleString()}</td>
                                </tr>
                            ))}
                            {items.length === 0 && (
                                <tr>
                                    <td colSpan={6} className="CatalogPage-empty">
                                        Студенты не найдены
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
