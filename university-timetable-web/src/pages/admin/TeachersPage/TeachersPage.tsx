import React, {useEffect, useState} from 'react';
import '../AdminCommon.css';
import './TeachersPage.css';
import {teachersApi} from '@/api/teachers';
import type {
    TeacherResponse,
    WorkingIntervalDto,
    UpdateTeacherRequest,
    UpdateWorkingHoursRequest,
} from '@/types/teacher';
import type {UUID} from '@/types/common';
import {FormErrorAlert} from '@/components/common/FormErrorAlert/FormErrorAlert';
import type {ApiErrorResponse} from '@/types/api-error';
import {parseApiErrorBody} from '@/types/api-error';
import {useAuth} from '@/hooks/useAuth';

const DAY_OPTIONS: WorkingIntervalDto['day'][] = [
    'MONDAY',
    'TUESDAY',
    'WEDNESDAY',
    'THURSDAY',
    'FRIDAY',
    'SATURDAY',
];

export const TeachersPage: React.FC = () => {
    const { role } = useAuth();
    const canEdit = role === 'ADMIN';

    const [teachers, setTeachers] = useState<TeacherResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [pageError, setPageError] = useState<string | null>(null);

    const [apiError, setApiError] = useState<ApiErrorResponse | null>(null);

    const [editModalOpen, setEditModalOpen] = useState(false);
    const [hoursModalOpen, setHoursModalOpen] = useState(false);

    const [editingTeacher, setEditingTeacher] = useState<TeacherResponse | null>(null);

    const [editFullName, setEditFullName] = useState('');
    const [editStatus, setEditStatus] = useState<'ACTIVE' | 'INACTIVE'>('ACTIVE');

    const [intervals, setIntervals] = useState<WorkingIntervalDto[]>([]);
    const [hoursSaving, setHoursSaving] = useState(false);
    const [editSaving, setEditSaving] = useState(false);

    const [newFullName, setNewFullName] = useState('');

    const loadTeachers = async () => {
        try {
            setLoading(true);
            setPageError(null);
            const data = await teachersApi.getAll();
            setTeachers(data);
        } catch (e) {
            setPageError('Не удалось загрузить преподавателей');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        void loadTeachers();
    }, []);

    const openEditModal = (t: TeacherResponse) => {
        setEditingTeacher(t);
        setEditFullName(t.fullName);
        setEditStatus(t.status);
        setApiError(null);
        setEditModalOpen(true);
    };

    const openHoursModal = (t: TeacherResponse) => {
        setEditingTeacher(t);
        setIntervals(t.preferredWorkingHours ?? []);
        setApiError(null);
        setHoursModalOpen(true);
    };

    const closeModals = () => {
        setEditingTeacher(null);
        setEditModalOpen(false);
        setHoursModalOpen(false);
        setApiError(null);
    };

    const handleArchive = async (t: TeacherResponse) => {
        try {
            setPageError(null);
            if (t.status === 'ACTIVE') {
                await teachersApi.archive(t.id as UUID);
            } else {
                await teachersApi.activate(t.id as UUID);
            }
            await loadTeachers();
        } catch {
            setPageError('Не удалось изменить статус преподавателя');
        }
    };

    const handleEditSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!editingTeacher) return;

        setEditSaving(true);
        setApiError(null);

        try {
            const payload: UpdateTeacherRequest = {
                fullName: editFullName.trim(),
            };
            await teachersApi.update(editingTeacher.id as UUID, payload);

            if (editingTeacher.status !== editStatus) {
                if (editStatus === 'ACTIVE') {
                    await teachersApi.activate(editingTeacher.id as UUID);
                } else {
                    await teachersApi.archive(editingTeacher.id as UUID);
                }
            }

            await loadTeachers();
            closeModals();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        } finally {
            setEditSaving(false);
        }
    };

    const handleHoursChange = (index: number, patch: Partial<WorkingIntervalDto>) => {
        setIntervals(prev =>
            prev.map((it, i) => (i === index ? {...it, ...patch} : it))
        );
    };

    const handleAddInterval = () => {
        setIntervals(prev => [
            ...prev,
            {
                day: 'MONDAY',
                startTime: '09:00',
                endTime: '10:30',
            },
        ]);
    };

    const handleRemoveInterval = (index: number) => {
        setIntervals(prev => prev.filter((_, i) => i !== index));
    };

    const handleHoursSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!editingTeacher) return;

        setHoursSaving(true);
        setApiError(null);

        try {
            const payload: UpdateWorkingHoursRequest = {
                preferredWorkingHours: intervals,
            };
            await teachersApi.updateWorkingHours(editingTeacher.id as UUID, payload);
            await loadTeachers();
            closeModals();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        } finally {
            setHoursSaving(false);
        }
    };

    const formatIntervals = (t: TeacherResponse) =>
        t.preferredWorkingHours.length === 0
            ? '—'
            : t.preferredWorkingHours
                .map(h => `${h.day} ${h.startTime}–${h.endTime}`)
                .join(', ');

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Преподаватели</h1>
                        <p className="Page-headerSubtitle">
                            Каталог преподавателей. Администратор может править данные и рабочие часы.
                        </p>
                    </div>
                </div>

                {canEdit && (
                    <form
                        className="SimpleForm"
                        onSubmit={async e => {
                            e.preventDefault();
                            if (!newFullName.trim()) return;
                            setApiError(null);
                            try {
                                await teachersApi.create({ fullName: newFullName.trim() });
                                setNewFullName('');
                                await loadTeachers();
                            } catch (err: any) {
                                const parsed = parseApiErrorBody(err.body);
                                if (parsed) setApiError(parsed);
                            }
                        }}
                    >
                        <input
                            type="text"
                            placeholder="ФИО"
                            value={newFullName}
                            onChange={e => setNewFullName(e.target.value)}
                        />
                        <button className="PrimaryButton" type="submit">
                            Добавить
                        </button>
                    </form>
                )}

                <FormErrorAlert apiError={apiError} />

                {pageError && <div className="AdminError">{pageError}</div>}
                {loading && <div>Загрузка...</div>}

                {!loading && !pageError && (
                    <div className="AdminTableWrapper">
                        <table className="AdminTable">
                            <thead>
                            <tr>
                                <th>ID</th>
                                <th>ФИО</th>
                                <th>Статус</th>
                                <th>Рабочие интервалы</th>
                                <th>Создан</th>
                                <th>Обновлён</th>
                                <th/>
                            </tr>
                            </thead>
                            <tbody>
                            {teachers.map(t => (
                                <tr key={t.id}>
                                    <td><code>{t.id}</code></td>
                                    <td>{t.fullName}</td>
                                    <td>{t.status}</td>
                                    <td>{formatIntervals(t)}</td>
                                    <td>{new Date(t.createdAt).toLocaleString()}</td>
                                    <td>{new Date(t.updatedAt).toLocaleString()}</td>
                                    <td className="AdminActionsCell">
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => openEditModal(t)}
                                        >
                                            Редактировать
                                        </button>
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => openHoursModal(t)}
                                        >
                                            Часы
                                        </button>
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => handleArchive(t)}
                                        >
                                            {t.status === 'ACTIVE' ? 'Архивировать' : 'Активировать'}
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {teachers.length === 0 && (
                                <tr>
                                    <td colSpan={7} className="AdminEmptyCell">
                                        Преподаватели не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {editModalOpen && editingTeacher && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <form className="AdminForm" onSubmit={handleEditSave}>
                                <h2>Редактирование преподавателя</h2>

                                <div className="AdminFormField">
                                    <span>ФИО</span>
                                    <input
                                        type="text"
                                        value={editFullName}
                                        onChange={e => setEditFullName(e.target.value)}
                                        required
                                    />
                                </div>

                                <div className="AdminFormField">
                                    <span>Статус</span>
                                    <select
                                        value={editStatus}
                                        onChange={e => setEditStatus(e.target.value as any)}
                                    >
                                        <option value="ACTIVE">ACTIVE</option>
                                        <option value="INACTIVE">INACTIVE</option>
                                    </select>
                                </div>

                                <FormErrorAlert apiError={apiError}/>

                                <div className="FormActions">
                                    <button
                                        type="button"
                                        className="SecondaryButton"
                                        onClick={closeModals}
                                    >
                                        Отмена
                                    </button>
                                    <button
                                        type="submit"
                                        className="PrimaryButton"
                                        disabled={editSaving}
                                    >
                                        {editSaving ? 'Сохранение...' : 'Сохранить'}
                                    </button>
                                </div>
                            </form>
                        </div>
                    </div>
                )}

                {hoursModalOpen && editingTeacher && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <form onSubmit={handleHoursSave}>
                                <h2>Рабочие часы: {editingTeacher.fullName}</h2>

                                <p style={{fontSize: 13, color: '#6b7280'}}>
                                    В один день интервалы не должны пересекаться, а начало должно быть раньше конца.
                                </p>

                                <div className="AdminForm">
                                    {intervals.map((it, idx) => (
                                        <div className="AdminFormRow" key={idx}>
                                            <div className="AdminFormField">
                                                <span>День</span>
                                                <select
                                                    value={it.day}
                                                    onChange={e =>
                                                        handleHoursChange(idx, {
                                                            day: e.target.value as WorkingIntervalDto['day'],
                                                        })
                                                    }
                                                >
                                                    {DAY_OPTIONS.map(d => (
                                                        <option key={d} value={d}>
                                                            {d}
                                                        </option>
                                                    ))}
                                                </select>
                                            </div>
                                            <div className="AdminFormField">
                                                <span>Начало</span>
                                                <input
                                                    type="time"
                                                    value={it.startTime}
                                                    onChange={e =>
                                                        handleHoursChange(idx, {startTime: e.target.value})
                                                    }
                                                    required
                                                />
                                            </div>
                                            <div className="AdminFormField">
                                                <span>Конец</span>
                                                <input
                                                    type="time"
                                                    value={it.endTime}
                                                    onChange={e =>
                                                        handleHoursChange(idx, {endTime: e.target.value})
                                                    }
                                                    required
                                                />
                                            </div>
                                            <div className="AdminFormField" style={{alignItems: 'flex-end'}}>
                                                <button
                                                    type="button"
                                                    className="SecondaryButton"
                                                    onClick={() => handleRemoveInterval(idx)}
                                                >
                                                    Удалить
                                                </button>
                                            </div>
                                        </div>
                                    ))}
                                </div>

                                <button
                                    type="button"
                                    className="SecondaryButton"
                                    onClick={handleAddInterval}
                                >
                                    + Добавить интервал
                                </button>

                                <FormErrorAlert apiError={apiError}/>

                                <div className="FormActions">
                                    <button
                                        type="button"
                                        className="SecondaryButton"
                                        onClick={closeModals}
                                    >
                                        Отмена
                                    </button>
                                    <button
                                        type="submit"
                                        className="PrimaryButton"
                                        disabled={hoursSaving}
                                    >
                                        {hoursSaving ? 'Сохранение...' : 'Сохранить'}
                                    </button>
                                </div>
                            </form>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};
