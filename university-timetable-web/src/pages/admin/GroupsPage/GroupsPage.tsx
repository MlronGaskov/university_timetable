import React, {useEffect, useState} from 'react';
import '../AdminCommon.css';
import './GroupsPage.css';
import {groupsApi} from '@/api/groups';
import type {
    GroupResponse,
    CreateGroupRequest,
    UpdateGroupRequest
} from '@/types/group';
import type {UUID} from '@/types/common';
import {FormErrorAlert} from '@/components/common/FormErrorAlert/FormErrorAlert';
import type {ApiErrorResponse} from '@/types/api-error';
import {parseApiErrorBody} from '@/types/api-error';
import {studentsApi} from "@/api/students";
import type {StudentResponse} from "@/types/student";

export const GroupsPage: React.FC = () => {
    const [groups, setGroups] = useState<GroupResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [pageError, setPageError] = useState<string | null>(null);
    const [apiError, setApiError] = useState<ApiErrorResponse | null>(null);

    const [name, setName] = useState('');
    const [code, setCode] = useState('');
    const [size, setSize] = useState(0);

    const [editModalOpen, setEditModalOpen] = useState(false);
    const [editingGroup, setEditingGroup] = useState<GroupResponse | null>(null);
    const [editName, setEditName] = useState('');
    const [editCode, setEditCode] = useState('');
    const [editSize, setEditSize] = useState(0);

    const [studentsModalOpen, setStudentsModalOpen] = useState(false);
    const [studentActionError, setStudentActionError] = useState<ApiErrorResponse | null>(null);
    const [studentIdToAdd, setStudentIdToAdd] = useState('');

    const [studentsById, setStudentsById] = useState<Record<string, StudentResponse>>({});
    const [studentsLoading, setStudentsLoading] = useState(false);

    const load = async () => {
        try {
            setLoading(true);
            setPageError(null);
            const data = await groupsApi.getAll();
            setGroups(data);
        } catch {
            setPageError('Не удалось загрузить группы');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        void load();
    }, []);

    const ensureStudentsLoaded = async () => {
        if (Object.keys(studentsById).length > 0) return;
        setStudentsLoading(true);
        try {
            const all = await studentsApi.getAll();
            const map: Record<string, StudentResponse> = {};
            all.forEach(s => {
                map[s.id] = s;
            });
            setStudentsById(map);
        } catch {
        } finally {
            setStudentsLoading(false);
        }
    };

    const handleCreate = async (e: React.FormEvent) => {
        e.preventDefault();
        setApiError(null);
        try {
            const payload: CreateGroupRequest = {name, code, size};
            await groupsApi.create(payload);
            setName('');
            setCode('');
            setSize(0);
            await load();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const openEditModal = (g: GroupResponse) => {
        setEditingGroup(g);
        setEditName(g.name);
        setEditCode(g.code);
        setEditSize(g.size);
        setApiError(null);
        setEditModalOpen(true);
    };

    const closeEditModal = () => {
        setEditModalOpen(false);
        setEditingGroup(null);
        setApiError(null);
    };

    const handleEditSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!editingGroup) return;

        setApiError(null);
        try {
            const payload: UpdateGroupRequest = {
                name: editName,
                code: editCode,
                size: editSize,
            };
            await groupsApi.update(editingGroup.id as UUID, payload);
            await load();
            closeEditModal();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const handleToggleStatus = async (g: GroupResponse) => {
        try {
            setPageError(null);
            if (g.status === 'ACTIVE') {
                await groupsApi.archive(g.id as UUID);
            } else {
                await groupsApi.activate(g.id as UUID);
            }
            await load();
        } catch {
            setPageError('Не удалось изменить статус группы');
        }
    };

    const openStudentsModal = async (g: GroupResponse) => {
        setEditingGroup(g);
        setStudentActionError(null);
        setStudentIdToAdd('');
        setStudentsModalOpen(true);
        await ensureStudentsLoaded();
    };

    const closeStudentsModal = () => {
        setStudentsModalOpen(false);
        setEditingGroup(null);
        setStudentActionError(null);
        setStudentIdToAdd('');
    };

    const handleAddStudent = async () => {
        if (!editingGroup || !studentIdToAdd.trim()) return;
        setStudentActionError(null);
        try {
            await groupsApi.addStudent(
                editingGroup.id as UUID,
                studentIdToAdd.trim() as UUID
            );
            await load();
            const updated = (await groupsApi.getOne(editingGroup.id as UUID)) as GroupResponse;
            setEditingGroup(updated);
            setStudentIdToAdd('');
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setStudentActionError(parsed);
        }
    };

    const handleRemoveStudent = async (studentId: UUID) => {
        if (!editingGroup) return;
        setStudentActionError(null);
        try {
            await groupsApi.removeStudent(editingGroup.id as UUID, studentId);
            await load();
            const updated = (await groupsApi.getOne(editingGroup.id as UUID)) as GroupResponse;
            setEditingGroup(updated);
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setStudentActionError(parsed);
        }
    };

    const renderStudentLabel = (id: string) => {
        const s = studentsById[id];
        if (!s) return <code>{id}</code>;
        return (
            <>
                <strong>{s.fullName}</strong>{' '}
                <span style={{fontSize: 12, color: '#6b7280'}}>
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
                            Каталог учебных групп. Администратор может редактировать, архивировать и управлять
                            списком студентов.
                        </p>
                    </div>
                </div>

                <form className="SimpleForm" onSubmit={handleCreate}>
                    <input
                        type="text"
                        placeholder="Название"
                        value={name}
                        onChange={e => setName(e.target.value)}
                    />
                    <input
                        type="text"
                        placeholder="Код"
                        value={code}
                        onChange={e => setCode(e.target.value)}
                    />
                    <input
                        type="number"
                        placeholder="Размер"
                        value={size}
                        onChange={e => setSize(Number(e.target.value))}
                    />
                    <button className="PrimaryButton" type="submit">
                        Добавить
                    </button>
                </form>

                <FormErrorAlert apiError={apiError}/>

                {pageError && <div className="AdminError">{pageError}</div>}
                {loading && <div>Загрузка...</div>}

                {!loading && !pageError && (
                    <div className="AdminTableWrapper">
                        <table className="AdminTable">
                            <thead>
                            <tr>
                                <th>Название</th>
                                <th>Код</th>
                                <th>Размер</th>
                                <th>Статус</th>
                                <th>Действия</th>
                            </tr>
                            </thead>
                            <tbody>
                            {groups.map(g => (
                                <tr key={g.id}>
                                    <td>{g.name}</td>
                                    <td>{g.code}</td>
                                    <td>{g.size}</td>
                                    <td>{g.status}</td>
                                    <td className="AdminActionsCell">
                                        <button
                                            className="AdminSmallButton"
                                            onClick={() => openEditModal(g)}
                                        >
                                            Редактировать
                                        </button>
                                        <button
                                            className="AdminSmallButton"
                                            style={{marginLeft: 6}}
                                            onClick={() => handleToggleStatus(g)}
                                        >
                                            {g.status === 'ACTIVE' ? 'Архивировать' : 'Активировать'}
                                        </button>
                                        <button
                                            className="AdminSmallButton"
                                            style={{marginLeft: 6}}
                                            onClick={() => openStudentsModal(g)}
                                        >
                                            Студенты
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {groups.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="AdminEmptyCell">
                                        Группы не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {editModalOpen && editingGroup && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <h2>Редактирование группы</h2>
                            <form onSubmit={handleEditSave} className="SimpleForm">
                                <input
                                    type="text"
                                    value={editName}
                                    onChange={e => setEditName(e.target.value)}
                                    placeholder="Название"
                                />
                                <input
                                    type="text"
                                    value={editCode}
                                    onChange={e => setEditCode(e.target.value)}
                                    placeholder="Код"
                                />
                                <input
                                    type="number"
                                    value={editSize}
                                    onChange={e => setEditSize(Number(e.target.value))}
                                    placeholder="Размер"
                                />
                                <FormErrorAlert apiError={apiError}/>
                                <div className="FormActions">
                                    <button
                                        type="button"
                                        className="SecondaryButton"
                                        onClick={closeEditModal}
                                    >
                                        Отмена
                                    </button>
                                    <button type="submit" className="PrimaryButton">
                                        Сохранить
                                    </button>
                                </div>
                            </form>
                        </div>
                    </div>
                )}

                {studentsModalOpen && editingGroup && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <h2>Студенты группы {editingGroup.name}</h2>

                            {studentsLoading && <div>Загрузка списка студентов...</div>}

                            {!studentsLoading && (
                                <>
                                    <div style={{marginBottom: 8}}>
                                        {editingGroup.studentIds.length === 0 ? (
                                            <div>В группе пока нет студентов.</div>
                                        ) : (
                                            <ul>
                                                {editingGroup.studentIds.map(id => (
                                                    <li key={id}>
                                                        {renderStudentLabel(id)}
                                                        <button
                                                            type="button"
                                                            className="AdminSmallButton"
                                                            style={{marginLeft: 8}}
                                                            onClick={() =>
                                                                handleRemoveStudent(id as UUID)
                                                            }
                                                        >
                                                            Удалить
                                                        </button>
                                                    </li>
                                                ))}
                                            </ul>
                                        )}
                                    </div>

                                    <div className="SimpleForm">
                                        <input
                                            type="text"
                                            placeholder="UUID студента"
                                            value={studentIdToAdd}
                                            onChange={e => setStudentIdToAdd(e.target.value)}
                                        />
                                        <button
                                            type="button"
                                            className="PrimaryButton"
                                            onClick={handleAddStudent}
                                        >
                                            Добавить
                                        </button>
                                    </div>

                                    {studentActionError && (
                                        <div style={{marginTop: 8}}>
                                            <FormErrorAlert apiError={studentActionError}/>
                                        </div>
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
