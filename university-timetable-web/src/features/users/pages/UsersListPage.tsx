import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {usersApi} from '@/api/users';
import type {Role, UserResponse} from '@/types/auth';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import {Input} from '@/components/ui/Input';
import {FormField} from '@/components/ui/FormField';
import {Button} from '@/components/ui/Button';
import {DataTable} from '@/components/ui/DataTable';
import {ActionsCell} from '@/components/ui/ActionsCell';
import {CsvMessages} from '@/components/ui/CsvMessages';
import {CsvToolbar} from '@/components/ui/CsvToolbar';
import {downloadCsv} from '@/utils/csv';
import crudStyles from '@/components/ui/CrudFormLayout.module.css';
import styles from './UsersListPage.module.css';

interface CsvRow {
    login: string;
    email: string;
    role: Role;
    teacherId: string | null;
    studentId: string | null;
    password: string | null;
    rowNumber: number;
}

type ImportTaskError = {rowNumber: number; message: string};

const runWithLimit = async <T,>(
    tasks: Array<() => Promise<T>>,
    limit: number,
): Promise<{results: T[]; errors: ImportTaskError[]}> => {
    const results: T[] = [];
    const errors: ImportTaskError[] = [];
    let i = 0;

    const workers = new Array(Math.max(1, limit)).fill(null).map(async () => {
        while (true) {
            const idx = i++;
            if (idx >= tasks.length) return;
            try {
                const r = await tasks[idx]();
                results.push(r);
            } catch (e: any) {
                const rowNumber = Number(e?.rowNumber ?? 0) || 0;
                const message = String(e?.message ?? e?.body?.message ?? 'ошибка импорта');
                errors.push({rowNumber, message});
            }
        }
    });

    await Promise.all(workers);
    return {results, errors};
};

export const UsersListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<UserResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);

    const [loginValue, setLoginValue] = useState('');
    const [email, setEmail] = useState('');
    const [role, setRole] = useState<Role>('STUDENT');
    const [password, setPassword] = useState('');
    const [teacherId, setTeacherId] = useState('');
    const [studentId, setStudentId] = useState('');
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    const [lastTempPassword, setLastTempPassword] = useState<string | null>(null);
    const [lastTempLogin, setLastTempLogin] = useState<string | null>(null);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await usersApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    const resetForm = () => {
        setFormMode(null);
        setEditingId(null);
        setLoginValue('');
        setEmail('');
        setRole('STUDENT');
        setPassword('');
        setTeacherId('');
        setStudentId('');
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (u: UserResponse) => {
        setFormMode('edit');
        setEditingId(u.id);
        setLoginValue(u.login);
        setEmail(u.email);
        setRole(u.role);
        setPassword('');
        setTeacherId(u.teacherId ?? '');
        setStudentId(u.studentId ?? '');
        setFormError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!loginValue.trim() || !email.trim() || !role) {
            setFormError('Заполните логин, email и роль');
            return;
        }

        setSaving(true);
        setFormError(null);
        setLastTempPassword(null);
        setLastTempLogin(null);

        try {
            if (formMode === 'create') {
                const body = {
                    login: loginValue.trim(),
                    email: email.trim(),
                    role,
                    password: password.trim() ? password.trim() : null,
                    teacherId: teacherId.trim() || null,
                    studentId: studentId.trim() || null,
                };
                const res = await usersApi.create(body);
                setItems(prev => [...prev, res.user]);

                if (res.tempPassword) {
                    setLastTempPassword(res.tempPassword);
                    setLastTempLogin(res.user.login);
                }
            } else if (formMode === 'edit' && editingId) {
                const updated = await usersApi.update(editingId, {
                    email: email.trim(),
                    role,
                    teacherId: teacherId.trim() || null,
                    studentId: studentId.trim() || null,
                });

                setItems(prev => prev.map(u => (u.id === updated.id ? updated : u)));

                const newPassword = password.trim();
                if (newPassword) {
                    try {
                        const updatedWithPassword = await usersApi.setPassword(editingId, {
                            newPassword: newPassword,
                        });
                        setItems(prev => prev.map(u => (u.id === updatedWithPassword.id ? updatedWithPassword : u)));
                    } catch (err) {
                        console.error(err);
                        setFormError('Пользователь обновлён, но пароль не удалось изменить');
                        return;
                    }
                }
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения пользователя');
        } finally {
            setSaving(false);
        }
    };

    const handleDeactivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await usersApi.deactivate(id);
            setItems(prev => prev.map(u => (u.id === updated.id ? updated : u)));
        } catch (e) {
            console.error(e);
            alert('Не удалось деактивировать пользователя');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await usersApi.activate(id);
            setItems(prev => prev.map(u => (u.id === updated.id ? updated : u)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать пользователя');
        }
    };

    const parseCsv = (text: string): CsvRow[] => {
        const lines = text.trim().split(/\r?\n/);
        if (lines.length < 2) return [];

        const header = lines[0].split(',').map(h => h.trim());
        if (header.length < 3 || header[0] !== 'login' || header[1] !== 'email' || header[2] !== 'role') {
            throw new Error('Ожидаются колонки как минимум: login,email,role[,teacherId,studentId,password]');
        }

        const idx = (name: string) => header.indexOf(name);

        const idxLogin = idx('login');
        const idxEmail = idx('email');
        const idxRole = idx('role');
        const idxTeacherId = idx('teacherId');
        const idxStudentId = idx('studentId');
        const idxPassword = idx('password');

        const rows: CsvRow[] = [];
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;
            const parts = line.split(',');

            const login = (parts[idxLogin] ?? '').trim();
            const email = (parts[idxEmail] ?? '').trim();
            const roleRaw = (parts[idxRole] ?? '').trim().toUpperCase() as Role;
            const teacherId = idxTeacherId >= 0 ? (parts[idxTeacherId] ?? '').trim() || null : null;
            const studentId = idxStudentId >= 0 ? (parts[idxStudentId] ?? '').trim() || null : null;
            const password = idxPassword >= 0 ? (parts[idxPassword] ?? '').trim() || null : null;

            rows.push({
                login,
                email,
                role: roleRaw,
                teacherId,
                studentId,
                password,
                rowNumber: i + 1,
            });
        }
        return rows;
    };

    const importFromFile = async (file: File) => {
        if (!isAdmin) return;

        setCsvError(null);
        setCsvResult(null);
        setCsvProcessing(true);

        try {
            const text = await file.text();
            const rows = parseCsv(text);

            if (rows.length === 0) {
                setCsvResult('Файл пуст или не содержит данных.');
                return;
            }

            const validationErrors: string[] = [];
            const tasks: Array<() => Promise<UserResponse>> = [];

            for (const row of rows) {
                if (!row.login || !row.email || !row.role) {
                    validationErrors.push(`Строка ${row.rowNumber}: не заполнены login/email/role`);
                    continue;
                }
                if (!['ADMIN', 'TEACHER', 'STUDENT'].includes(row.role)) {
                    validationErrors.push(`Строка ${row.rowNumber}: некорректная роль "${row.role}"`);
                    continue;
                }

                tasks.push(async () => {
                    try {
                        const createdRes = await usersApi.create({
                            login: row.login,
                            email: row.email,
                            role: row.role,
                            password: row.password,
                            teacherId: row.teacherId,
                            studentId: row.studentId,
                        });
                        return createdRes.user;
                    } catch (err: any) {
                        const msg =
                            err?.body?.message ??
                            (typeof err?.message === 'string' ? err.message : null) ??
                            'ошибка создания';
                        throw {rowNumber: row.rowNumber, message: msg};
                    }
                });
            }

            const {results: createdUsers, errors: taskErrors} = await runWithLimit(tasks, 8);

            if (createdUsers.length > 0) {
                setItems(prev => [...prev, ...createdUsers]);
            }

            const errors: string[] = [...validationErrors];
            for (const e of taskErrors) {
                errors.push(`Строка ${e.rowNumber}: ${e.message}`);
            }

            const summary = [
                `Успешно создано: ${createdUsers.length}`,
                errors.length ? `Ошибок: ${errors.length}` : '',
                errors.length ? `\n${errors.join('\n')}` : '',
            ]
                .filter(Boolean)
                .join('\n');

            setCsvResult(summary);
        } catch (err: any) {
            console.error(err);
            setCsvError(err.message || 'Ошибка чтения или парсинга CSV');
        } finally {
            setCsvProcessing(false);
        }
    };

    const exportCsv = () => {
        downloadCsv(
            'users.csv',
            ['login', 'email', 'role', 'status', 'teacherId', 'studentId', 'createdAt', 'updatedAt'],
            items.map(u => [
                u.login,
                u.email,
                u.role,
                u.status,
                u.teacherId ?? '',
                u.studentId ?? '',
                u.createdAt,
                u.updatedAt,
            ]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать пользователя"
            onCreate={openCreateForm}
        />
    );

    const getUserFio = (u: UserResponse): string => {
        const anyU = u as any;
        return (
            anyU.fullName ??
            u.teacherName ??
            u.studentName ??
            '—'
        );
    };

    return (
        <Page title="Пользователи" actions={actions}>
            <CsvMessages error={csvError} result={csvResult} />

            {lastTempPassword && lastTempLogin && (
                <p className={styles.tempPassword}>
                    Временный пароль для пользователя <b>{lastTempLogin}</b>: <code>{lastTempPassword}</code>
                    <br />
                    Сохраните и передайте его пользователю, повторно он показан не будет.
                </p>
            )}

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Логин">
                        <Input value={loginValue} onChange={e => setLoginValue(e.target.value)} disabled={formMode === 'edit'} />
                    </FormField>

                    <FormField label="Email">
                        <Input type="email" value={email} onChange={e => setEmail(e.target.value)} />
                    </FormField>

                    <FormField label="Роль">
                        <select
                            value={role}
                            onChange={e => setRole(e.target.value as Role)}
                            style={{
                                padding: '8px 10px',
                                borderRadius: 10,
                                border: '1px solid var(--color-border-subtle)',
                                background: '#ffffff',
                                fontSize: 14,
                            }}
                        >
                            <option value="ADMIN">ADMIN</option>
                            <option value="TEACHER">TEACHER</option>
                            <option value="STUDENT">STUDENT</option>
                        </select>
                    </FormField>

                    <FormField label="Teacher ID (опционально)">
                        <Input value={teacherId} onChange={e => setTeacherId(e.target.value)} />
                    </FormField>

                    <FormField label="Student ID (опционально)">
                        <Input value={studentId} onChange={e => setStudentId(e.target.value)} />
                    </FormField>

                    <FormField label="Пароль (опционально)">
                        <Input
                            type="text"
                            placeholder="Оставьте пустым для автогенерации"
                            value={password}
                            onChange={e => setPassword(e.target.value)}
                        />
                    </FormField>

                    <div className={crudStyles.formActions}>
                        <Button type="submit" disabled={saving}>
                            {saving ? 'Сохранение…' : formMode === 'create' ? 'Создать' : 'Сохранить'}
                        </Button>
                        <Button type="button" variant="ghost" onClick={resetForm}>
                            Отмена
                        </Button>
                    </div>

                    {formError && <div className={crudStyles.formError}>{formError}</div>}
                </form>
            )}

            {loading && <p>Загрузка…</p>}

            {!loading && (
                <DataTable>
                    <thead>
                    <tr>
                        <th align="left">Логин</th>
                        <th align="left">Email</th>
                        <th align="left">Роль</th>
                        <th align="left">Статус</th>
                        <th align="left">ФИО</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(u => (
                        <tr key={u.id}>
                            <td>{u.login}</td>
                            <td>{u.email}</td>
                            <td>{u.role}</td>
                            <td>{u.status}</td>
                            <td>{getUserFio(u)}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button variant="ghost" type="button" onClick={() => openEditForm(u)}>
                                        Редактировать
                                    </Button>
                                    {u.status === 'ACTIVE' ? (
                                        <Button variant="ghost" type="button" onClick={() => handleDeactivate(u.id)}>
                                            Деактивировать
                                        </Button>
                                    ) : (
                                        <Button variant="ghost" type="button" onClick={() => handleActivate(u.id)}>
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 6 : 5}>Нет пользователей</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
