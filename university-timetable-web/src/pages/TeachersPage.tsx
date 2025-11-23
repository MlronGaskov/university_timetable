import React, {useEffect, useState} from 'react';
import {useNavigate} from 'react-router-dom';
import Button from '../components/ui/Button';
import Modal from '../components/ui/Modal';
import Input from '../components/ui/Input';
import {TeacherDto} from '@/types';
import {TeachersApi, CreateTeacherReq, UpdateTeacherReq} from '@api/teachers';
import {useAuth} from '@hooks/useAuth';
import NavBar from "@components/NavBar";

type TeacherFormProps = {
    initial?: TeacherDto;
    onSubmit: (data: CreateTeacherReq | UpdateTeacherReq) => Promise<void> | void;
    submitLabel: string;
};

function TeacherForm({initial, onSubmit, submitLabel}: TeacherFormProps) {
    const isEdit = !!initial;
    const [fullName, setFullName] = useState(initial?.fullName ?? '');
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setSaving(true);
        setError(null);
        try {
            const body: any = {fullName: fullName.trim()};
            await onSubmit(body);
        } catch (err: any) {
            setError(err?.message ?? 'Ошибка');
        } finally {
            setSaving(false);
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            <div className="row">
                <label className="label">ФИО преподавателя</label>
                <Input
                    value={fullName}
                    onChange={e => setFullName(e.currentTarget.value)}
                    required
                />
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

export default function TeachersPage() {
    const {isAdmin} = useAuth();
    const [teachers, setTeachers] = useState<TeacherDto[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [showCreate, setShowCreate] = useState(false);
    const [editing, setEditing] = useState<TeacherDto | null>(null);

    const load = async () => {
        setLoading(true);
        setError(null);
        try {
            setTeachers(await TeachersApi.list());
        } catch {
            setError('Ошибка загрузки преподавателей');
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
                <div className="page-header-title">Преподаватели</div>
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
                                <th>Статус</th>
                                <th>Создано</th>
                                <th>Обновлено</th>
                                {isAdmin && <th>Действия</th>}
                            </tr>
                            </thead>
                            <tbody>
                            {teachers.map(t => (
                                <tr key={t.id}>
                                    <td>{t.fullName}</td>
                                    <td>{t.status}</td>
                                    <td>{new Date(t.createdAt).toLocaleString()}</td>
                                    <td>{new Date(t.updatedAt).toLocaleString()}</td>
                                    {isAdmin && (
                                        <td style={{textAlign: 'center'}}>
                                            <div
                                                style={{display: 'flex', gap: 8, justifyContent: 'center', flexWrap: 'wrap'}}
                                            >
                                                <Button onClick={() => setEditing(t)}>Редактировать</Button>
                                                <Button
                                                    className="warn"
                                                    disabled={t.status === 'INACTIVE'}
                                                    onClick={async () => {
                                                        if (!window.confirm('Архивировать преподавателя?')) return;
                                                        await TeachersApi.archive(t.id);
                                                        load();
                                                    }}
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
                <Modal title="Создать преподавателя" onClose={() => setShowCreate(false)}>
                    <TeacherForm
                        submitLabel="Создать"
                        onSubmit={async data => {
                            await TeachersApi.create(data as CreateTeacherReq);
                            setShowCreate(false);
                            load();
                        }}
                    />
                </Modal>
            )}

            {editing && (
                <Modal title={`Редактировать: ${editing.fullName}`} onClose={() => setEditing(null)}>
                    <TeacherForm
                        initial={editing}
                        submitLabel="Сохранить"
                        onSubmit={async data => {
                            await TeachersApi.update(editing.id, data as UpdateTeacherReq);
                            setEditing(null);
                            load();
                        }}
                    />
                </Modal>
            )}
        </div>
    );
}
