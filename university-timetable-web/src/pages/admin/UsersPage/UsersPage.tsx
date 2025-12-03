import React, {useEffect, useState} from 'react';
import '../AdminCommon.css';
import './UsersPage.css';
import {usersApi} from '@/api/users';
import type {UserResponse} from '@/types/auth';
import type {UUID} from '@/types/common';
import {UserForm} from './UserForm';
import {parseApiErrorBody} from '@/types/api-error';
import type {ApiErrorResponse} from '@/types/api-error';

export const UsersPage: React.FC = () => {
    const [users, setUsers] = useState<UserResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [apiError, setApiError] = useState<ApiErrorResponse | null>(null);

    const [modalOpen, setModalOpen] = useState(false);
    const [editingUser, setEditingUser] = useState<UserResponse | null>(null);

    const loadUsers = async () => {
        try {
            setLoading(true);
            setError(null);
            const data = await usersApi.getAll();
            setUsers(data);
        } catch {
            setError('Не удалось загрузить пользователей');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        void loadUsers();
    }, []);

    const handleCreate = () => {
        setEditingUser(null);
        setApiError(null);
        setModalOpen(true);
    };

    const handleEdit = (u: UserResponse) => {
        setEditingUser(u);
        setApiError(null);
        setModalOpen(true);
    };

    const handleSaved = async () => {
        await loadUsers();
    };

    const handleError = (err: unknown) => {
        const httpErr = err as any;
        const parsed = parseApiErrorBody(httpErr.body);
        if (parsed) setApiError(parsed);
        else setApiError(null);
    };

    const handleToggleStatus = async (u: UserResponse) => {
        setApiError(null);
        try {
            if (u.status === 'ACTIVE') {
                await usersApi.deactivate(u.id as UUID);
            } else {
                await usersApi.activate(u.id as UUID);
            }
            await loadUsers();
        } catch (err) {
            handleError(err);
        }
    };

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Пользователи</h1>
                        <p className="Page-headerSubtitle">
                            Управление учётными записями, ролями и связями с преподавателями и студентами.
                        </p>
                    </div>
                    <button className="PrimaryButton" onClick={handleCreate}>
                        + Новый пользователь
                    </button>
                </div>

                {loading && <div>Загрузка...</div>}
                {error && <div className="AdminError">{error}</div>}

                {!loading && !error && (
                    <div className="AdminTableWrapper">
                        <table className="AdminTable">
                            <thead>
                            <tr>
                                <th>ID</th>
                                <th>Логин</th>
                                <th>Email</th>
                                <th>Роль</th>
                                <th>Статус</th>
                                <th>Учитель</th>
                                <th>Студент</th>
                                <th>Создан</th>
                                <th>Обновлён</th>
                                <th/>
                            </tr>
                            </thead>
                            <tbody>
                            {users.map(u => (
                                <tr key={u.id}>
                                    <td><code>{u.id}</code></td>
                                    <td>{u.login}</td>
                                    <td>{u.email}</td>
                                    <td>{u.role}</td>
                                    <td>{u.status}</td>
                                    <td>{u.teacherName ?? '—'}</td>
                                    <td>{u.studentName ?? '—'}</td>
                                    <td>{new Date(u.createdAt).toLocaleString()}</td>
                                    <td>{new Date(u.updatedAt).toLocaleString()}</td>
                                    <td className="AdminActionsCell">
                                        <button
                                            className="AdminSmallButton"
                                            onClick={() => handleEdit(u)}
                                        >
                                            Редактировать
                                        </button>
                                        <button
                                            className="AdminSmallButton"
                                            style={{marginLeft: 6}}
                                            onClick={() => handleToggleStatus(u)}
                                        >
                                            {u.status === 'ACTIVE' ? 'Деактивировать' : 'Активировать'}
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {users.length === 0 && (
                                <tr>
                                    <td colSpan={10} className="AdminEmptyCell">
                                        Пользователи не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {modalOpen && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <UserForm
                                mode={editingUser ? 'edit' : 'create'}
                                initialUser={editingUser ?? undefined}
                                onSaved={handleSaved}
                                onCancel={() => {
                                    setModalOpen(false);
                                    setEditingUser(null);
                                    setApiError(null);
                                }}
                                onError={handleError}
                                apiError={apiError}
                            />
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};
