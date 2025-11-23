import React, {useEffect, useState} from 'react';
import Button from '../components/ui/Button';
import Modal from '../components/ui/Modal';
import UsersTable from '../components/users/UsersTable';
import UserForm from '../components/users/UserForm';
import NavBar from '../components/NavBar';
import {UserDto} from '@/types';
import {UsersApi} from '@api/users';

export default function UsersPage() {
    const [users, setUsers] = useState<UserDto[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [showCreate, setShowCreate] = useState(false);
    const [editing, setEditing] = useState<UserDto | null>(null);
    const [passwordInfo, setPasswordInfo] = useState<{ login: string; tempPassword: string } | null>(null);

    const load = async () => {
        setLoading(true);
        setError(null);
        try {
            setUsers(await UsersApi.list());
        } catch {
            setError('Ошибка загрузки пользователей');
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
                <div className="page-header-title">Пользователи</div>
                <div style={{display: 'flex', gap: 8, alignItems: 'center'}}>
                    <NavBar/>
                    <Button onClick={() => setShowCreate(true)} className="primary">Создать</Button>
                </div>
            </header>

            <main className="container page-main">
                {loading && <div>Загрузка…</div>}
                {error && <div className="error">{error}</div>}
                {!loading && (
                    <UsersTable
                        users={users}
                        onEdit={(u) => setEditing(u)}
                        onToggle={async (u) => {
                            u.status === 'ACTIVE' ? await UsersApi.deactivate(u.id) : await UsersApi.activate(u.id);
                            load();
                        }}
                        onChangePassword={async (u) => {
                            const np = prompt(`Новый пароль для ${u.login}:`, '');
                            if (!np) return;
                            try {
                                await UsersApi.setPassword(u.id, np);
                                alert('Пароль обновлён');
                                load();
                            } catch (e: any) {
                                alert(e?.message ?? 'Ошибка смены пароля');
                            }
                        }}
                    />
                )}
            </main>

            {showCreate && (
                <Modal title="Создать пользователя" onClose={() => setShowCreate(false)}>
                    <UserForm
                        submitLabel="Создать"
                        onSubmit={async (f) => {
                            const res = await UsersApi.create({
                                login: f.login,
                                email: f.email,
                                role: f.role,
                                password: f.password || undefined,
                            });
                            setShowCreate(false);
                            if (res.tempPassword) {
                                setPasswordInfo({login: res.user.login, tempPassword: res.tempPassword});
                            }
                            load();
                        }}
                    />
                </Modal>
            )}

            {editing && (
                <Modal title={`Редактировать: ${editing.login}`} onClose={() => setEditing(null)}>
                    <UserForm
                        initial={editing}
                        submitLabel="Сохранить"
                        onSubmit={async (f) => {
                            await UsersApi.update(editing.id, {email: f.email, role: f.role, status: f.status});
                            setEditing(null);
                            load();
                        }}
                    />
                </Modal>
            )}

            {passwordInfo && (
                <Modal title={`Временный пароль для ${passwordInfo.login}`} onClose={() => setPasswordInfo(null)}>
                    <div className="row">
                        <p>Сохраните этот пароль и передайте пользователю. Он показан только один раз:</p>
                        <pre style={{userSelect: 'all', padding: 12, background: '#eee', borderRadius: 8}}>
                            {passwordInfo.tempPassword}
                        </pre>
                        <Button onClick={() => {
                            navigator.clipboard.writeText(passwordInfo.tempPassword);
                            alert('Скопировано');
                        }}>Скопировать</Button>
                    </div>
                </Modal>
            )}
        </div>
    );
}
