import React, {useEffect, useState} from "react";
import {groupsApi} from "@/api/groups";
import type {GroupResponse} from "@/types/group";
import "../CatalogPage.css";
import {studentsApi} from "@/api/students";
import type {StudentResponse} from "@/types/student";

export const GroupsCatalogPage: React.FC = () => {
    const [items, setItems] = useState<GroupResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    const [studentsById, setStudentsById] = useState<Record<string, StudentResponse>>({});
    const [studentsLoading, setStudentsLoading] = useState(false);

    const [selectedGroup, setSelectedGroup] = useState<GroupResponse | null>(null);
    const [studentsModalOpen, setStudentsModalOpen] = useState(false);

    useEffect(() => {
        const load = async () => {
            try {
                setError(null);
                setLoading(true);
                setStudentsLoading(true);

                const [groups, students] = await Promise.all([
                    groupsApi.getAll(),
                    studentsApi
                        .getAll()
                        .catch(() => [] as StudentResponse[]),
                ]);

                setItems(groups);

                const map: Record<string, StudentResponse> = {};
                students.forEach(s => {
                    map[s.id] = s;
                });
                setStudentsById(map);
            } catch (e) {
                setError("Не удалось загрузить группы");
            } finally {
                setLoading(false);
                setStudentsLoading(false);
            }
        };
        void load();
    }, []);

    const openStudentsModal = (g: GroupResponse) => {
        setSelectedGroup(g);
        setStudentsModalOpen(true);
    };

    const closeStudentsModal = () => {
        setStudentsModalOpen(false);
        setSelectedGroup(null);
    };

    const renderStudentLabel = (id: string) => {
        const s = studentsById[id];
        if (!s) return <code>{id}</code>;
        return (
            <>
                <strong>{s.fullName}</strong>{" "}
                <span style={{fontSize: 12, color: "#6b7280"}}>
                    (<code>{id}</code>)
                </span>
            </>
        );
    };

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Группы</h1>
                        <p className="Page-headerSubtitle">
                            Справочник учебных групп. Редактирование доступно только администратору,
                            но список студентов виден всем ролям (только просмотр).
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
                                <th>Название</th>
                                <th>Код</th>
                                <th>Размер</th>
                                <th>Статус</th>
                                <th>Студентов</th>
                                <th/>
                            </tr>
                            </thead>
                            <tbody>
                            {items.map(g => (
                                <tr key={g.id}>
                                    <td>{g.name}</td>
                                    <td>{g.code}</td>
                                    <td>{g.size}</td>
                                    <td>{g.status}</td>
                                    <td>{g.studentIds?.length ?? 0}</td>
                                    <td>
                                        {(g.studentIds?.length ?? 0) > 0 && (
                                            <button
                                                type="button"
                                                className="SecondaryButton"
                                                onClick={() => openStudentsModal(g)}
                                            >
                                                Студенты
                                            </button>
                                        )}
                                    </td>
                                </tr>
                            ))}
                            {items.length === 0 && (
                                <tr>
                                    <td colSpan={6} className="CatalogPage-empty">
                                        Группы не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {studentsModalOpen && selectedGroup && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <h2>Студенты группы {selectedGroup.name}</h2>

                            {studentsLoading && <div>Загрузка списка студентов...</div>}

                            {!studentsLoading && (
                                <>
                                    {selectedGroup.studentIds.length === 0 ? (
                                        <div>В этой группе пока нет студентов.</div>
                                    ) : (
                                        <ul>
                                            {selectedGroup.studentIds.map(id => (
                                                <li key={id}>{renderStudentLabel(id)}</li>
                                            ))}
                                        </ul>
                                    )}

                                    <div className="FormActions" style={{marginTop: 12}}>
                                        <button
                                            type="button"
                                            className="SecondaryButton"
                                            onClick={closeStudentsModal}
                                        >
                                            Закрыть
                                        </button>
                                    </div>
                                </>
                            )}
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};
