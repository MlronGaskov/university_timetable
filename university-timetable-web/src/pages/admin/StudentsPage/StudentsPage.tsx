import React, {useEffect, useState} from 'react';
import '../AdminCommon.css';
import './StudentsPage.css';
import {studentsApi} from '@/api/students';
import type {StudentResponse, CreateStudentRequest, UpdateStudentRequest} from '@/types/student';
import type {UUID} from '@/types/common';
import {FormErrorAlert} from '@/components/common/FormErrorAlert/FormErrorAlert';
import type {ApiErrorResponse} from '@/types/api-error';
import {parseApiErrorBody} from '@/types/api-error';

export const StudentsPage: React.FC = () => {
    const [students, setStudents] = useState<StudentResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [apiError, setApiError] = useState<ApiErrorResponse | null>(null);
    const [pageError, setPageError] = useState<string | null>(null);

    const [fullName, setFullName] = useState('');
    const [studentId, setStudentId] = useState('');

    const [editModalOpen, setEditModalOpen] = useState(false);
    const [editingStudent, setEditingStudent] = useState<StudentResponse | null>(null);
    const [editFullName, setEditFullName] = useState('');
    const [editStudentId, setEditStudentId] = useState('');
    const [editStatus, setEditStatus] = useState<'ACTIVE' | 'INACTIVE'>('ACTIVE');

    const load = async () => {
        try {
            setLoading(true);
            setPageError(null);
            const data = await studentsApi.getAll();
            setStudents(data);
        } catch {
            setPageError('Не удалось загрузить студентов');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        void load();
    }, []);

    const handleCreate = async (e: React.FormEvent) => {
        e.preventDefault();
        setApiError(null);
        try {
            const payload: CreateStudentRequest = {
                fullName,
                studentId,
            };
            await studentsApi.create(payload);
            setFullName('');
            setStudentId('');
            await load();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const openEditModal = (s: StudentResponse) => {
        setEditingStudent(s);
        setEditFullName(s.fullName);
        setEditStudentId(s.studentId);
        setEditStatus(s.status);
        setApiError(null);
        setEditModalOpen(true);
    };

    const closeEditModal = () => {
        setEditModalOpen(false);
        setEditingStudent(null);
        setApiError(null);
    };

    const handleEditSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!editingStudent) return;

        setApiError(null);
        try {
            const payload: UpdateStudentRequest = {
                fullName: editFullName,
                studentId: editStudentId,
            };
            await studentsApi.update(editingStudent.id as UUID, payload);

            if (editingStudent.status !== editStatus) {
                if (editStatus === 'ACTIVE') {
                    await studentsApi.activate(editingStudent.id as UUID);
                } else {
                    await studentsApi.archive(editingStudent.id as UUID);
                }
            }

            await load();
            closeEditModal();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const handleToggleStatus = async (s: StudentResponse) => {
        try {
            setPageError(null);
            if (s.status === 'ACTIVE') {
                await studentsApi.archive(s.id as UUID);
            } else {
                await studentsApi.activate(s.id as UUID);
            }
            await load();
        } catch {
            setPageError('Не удалось изменить статус студента');
        }
    };

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Студенты</h1>
                        <p className="Page-headerSubtitle">Каталог студентов для администратора.</p>
                    </div>
                </div>

                <form className="SimpleForm" onSubmit={handleCreate}>
                    <input
                        type="text"
                        placeholder="ФИО"
                        value={fullName}
                        onChange={e => setFullName(e.target.value)}
                    />
                    <input
                        type="text"
                        placeholder="studentId"
                        value={studentId}
                        onChange={e => setStudentId(e.target.value)}
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
                                <th>ID</th>
                                <th>ФИО</th>
                                <th>studentId</th>
                                <th>Статус</th>
                                <th>Создан</th>
                                <th>Обновлён</th>
                                <th/>
                            </tr>
                            </thead>
                            <tbody>
                            {students.map(s => (
                                <tr key={s.id}>
                                    <td><code>{s.id}</code></td>
                                    <td>{s.fullName}</td>
                                    <td>{s.studentId}</td>
                                    <td>{s.status}</td>
                                    <td>{new Date(s.createdAt).toLocaleString()}</td>
                                    <td>{new Date(s.updatedAt).toLocaleString()}</td>
                                    <td className="AdminActionsCell">
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => openEditModal(s)}
                                        >
                                            Редактировать
                                        </button>
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => handleToggleStatus(s)}
                                        >
                                            {s.status === 'ACTIVE' ? 'Архивировать' : 'Активировать'}
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {students.length === 0 && (
                                <tr>
                                    <td colSpan={7} className="AdminEmptyCell">
                                        Студенты не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {editModalOpen && editingStudent && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <form onSubmit={handleEditSave}>
                                <h2>Редактирование студента</h2>

                                <div className="AdminFormField">
                                    <span>ФИО</span>
                                    <input
                                        type="text"
                                        value={editFullName}
                                        onChange={e => setEditFullName(e.target.value)}
                                    />
                                </div>

                                <div className="AdminFormField">
                                    <span>studentId</span>
                                    <input
                                        type="text"
                                        value={editStudentId}
                                        onChange={e => setEditStudentId(e.target.value)}
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
            </div>
        </div>
    );
};
