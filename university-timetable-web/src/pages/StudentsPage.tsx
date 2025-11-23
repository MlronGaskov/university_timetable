import React, {useEffect, useState} from 'react';
import {useNavigate} from 'react-router-dom';
import Button from '../components/ui/Button';
import Modal from '../components/ui/Modal';
import Input from '../components/ui/Input';
import Select from '../components/ui/Select';
import {StudentDto, GroupDto} from '@/types';
import {StudentsApi, CreateStudentReq, UpdateStudentReq} from '@api/students';
import {GroupsApi} from '@api/groups';
import {useAuth} from '@hooks/useAuth';
import NavBar from "@components/NavBar";

function StudentForm({initial, groups, onSubmit, submitLabel}: {
    initial?: StudentDto;
    groups: GroupDto[];
    onSubmit: (data: CreateStudentReq | UpdateStudentReq) => Promise<void> | void;
    submitLabel: string;
}) {
    const isEdit = !!initial;
    const [fullName, setFullName] = useState(initial?.fullName ?? '');
    const [studentId, setStudentId] = useState(initial?.studentId ?? '');
    const [groupId, setGroupId] = useState<string | ''>(initial?.groupId ?? '');
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setSaving(true);
        setError(null);
        try {
            const body: any = {
                fullName: fullName.trim(),
                studentId: studentId.trim(),
                groupId: groupId || null,
            };
            await onSubmit(body);
        } catch (err: any) {
            setError(err?.message ?? 'Ошибка');
        } finally {
            setSaving(false);
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            {!isEdit && (
                <div className="row">
                    <label className="label">ФИО</label>
                    <Input value={fullName} onChange={e => setFullName(e.currentTarget.value)} required/>
                </div>
            )}
            {isEdit && (
                <div className="row">
                    <label className="label">ФИО</label>
                    <Input value={fullName} onChange={e => setFullName(e.currentTarget.value)} required/>
                </div>
            )}
            <div className="row">
                <label className="label">Студенческий ID</label>
                <Input value={studentId} onChange={e => setStudentId(e.currentTarget.value)} required/>
            </div>
            <div className="row">
                <label className="label">Группа</label>
                <Select value={groupId} onChange={e => setGroupId(e.currentTarget.value)}>
                    <option value="">(без группы)</option>
                    {groups.map(g => (
                        <option key={g.id} value={g.id}>{g.code} – {g.name}</option>
                    ))}
                </Select>
            </div>
            {error && <div className="error">{error}</div>}
            <div style={{display: 'flex', justifyContent: 'flex-end', gap: 8, paddingTop: 8}}>
                <Button type="submit" className="primary" disabled={saving}>
                    {submitLabel}
                </Button>
            </div>
        </form>
    );
}

export default function StudentsPage() {
    const {isAdmin} = useAuth();
    const [students, setStudents] = useState<StudentDto[]>([]);
    const [groups, setGroups] = useState<GroupDto[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [showCreate, setShowCreate] = useState(false);
    const [editing, setEditing] = useState<StudentDto | null>(null);

    const load = async () => {
        setLoading(true);
        setError(null);
        try {
            const [st, gr] = await Promise.all([StudentsApi.list(), GroupsApi.list()]);
            setStudents(st);
            setGroups(gr);
        } catch {
            setError('Ошибка загрузки студентов');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        load();
    }, []);

    return (
        <div className="page">
            <header className="hdr">
                <div className="page-header-title">Студенты</div>
                <div style={{display: 'flex', gap: 8, alignItems: 'center'}}>
                    <NavBar/>
                    {isAdmin && (
                        <Button onClick={() => setShowCreate(true)} className="primary">
                            Создать
                        </Button>
                    )}
                </div>
            </header>

            <main className="container page-main">
                {loading && <div>Загрузка…</div>}
                {error && <div className="error">{error}</div>}
                {!loading && (
                    <div className="card" style={{overflow: 'auto'}}>
                        <table className="table">
                            <thead>
                            <tr>
                                <th>ФИО</th>
                                <th>ID</th>
                                <th>Группа</th>
                                <th>Статус</th>
                                <th>Создано</th>
                                <th>Обновлено</th>
                                {isAdmin && <th>Действия</th>}
                            </tr>
                            </thead>
                            <tbody>
                            {students.map(s => (
                                <tr key={s.id}>
                                    <td>{s.fullName}</td>
                                    <td>{s.studentId}</td>
                                    <td>{s.groupName ?? '—'}</td>
                                    <td>{s.status}</td>
                                    <td>{new Date(s.createdAt).toLocaleString()}</td>
                                    <td>{new Date(s.updatedAt).toLocaleString()}</td>
                                    {isAdmin && (
                                        <td style={{textAlign: 'center'}}>
                                            <div style={{
                                                display: 'flex',
                                                gap: 8,
                                                justifyContent: 'center',
                                                flexWrap: 'wrap'
                                            }}>
                                                <Button onClick={() => setEditing(s)}>Редактировать</Button>
                                                <Button
                                                    className="warn"
                                                    onClick={async () => {
                                                        if (!window.confirm('Архивировать студента?')) return;
                                                        await StudentsApi.archive(s.id);
                                                        load();
                                                    }}
                                                    disabled={s.status === 'INACTIVE'}
                                                >
                                                    Архивировать
                                                </Button>
                                            </div>
                                        </td>
                                    )}
                                </tr>
                            ))}
                            </tbody>
                        </table>
                    </div>
                )}
            </main>

            {showCreate && (
                <Modal title="Создать студента" onClose={() => setShowCreate(false)}>
                    <StudentForm
                        groups={groups}
                        submitLabel="Создать"
                        onSubmit={async data => {
                            await StudentsApi.create(data as CreateStudentReq);
                            setShowCreate(false);
                            load();
                        }}
                    />
                </Modal>
            )}

            {editing && (
                <Modal title={`Редактировать: ${editing.fullName}`} onClose={() => setEditing(null)}>
                    <StudentForm
                        initial={editing}
                        groups={groups}
                        submitLabel="Сохранить"
                        onSubmit={async data => {
                            await StudentsApi.update(editing.id, data as UpdateStudentReq);
                            setEditing(null);
                            load();
                        }}
                    />
                </Modal>
            )}
        </div>
    );
}
