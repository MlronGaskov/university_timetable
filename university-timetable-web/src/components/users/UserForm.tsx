import React, {useState} from 'react';
import Input from '../ui/Input';
import Select from '../ui/Select';
import Button from '../ui/Button';
import {Role, Status, UserDto} from '@/types';

export default function UserForm({initial, onSubmit, submitLabel}: {
    initial?: Partial<UserDto>;
    onSubmit: (u: any) => Promise<void> | void;
    submitLabel: string;
}) {
    const [form, setForm] = useState({
        login: initial?.login ?? '',
        email: initial?.email ?? '',
        role: (initial?.role as Role) ?? 'STUDENT',
        status: (initial?.status as Status) ?? 'ACTIVE',
        password: ''
    });
    const isEdit = !!initial?.id;
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const submit = async (e: React.FormEvent) => {
        e.preventDefault();
        setSaving(true);
        setError(null);
        try {
            await onSubmit(form);
        } catch (e: any) {
            setError(e.message ?? 'Ошибка');
        } finally {
            setSaving(false);
        }
    };

    return (
        <form onSubmit={submit}>
            {!isEdit && (
                <div className="row">
                    <label className="label">Логин</label>
                    <Input
                        value={form.login}
                        onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                            setForm({...form, login: e.currentTarget.value})
                        }
                        required
                    />
                </div>
            )}
            <div className="row">
                <label className="label">Email</label>
                <Input
                    type="email"
                    value={form.email}
                    onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                        setForm({...form, email: e.currentTarget.value})
                    }
                    required
                />
            </div>
            <div className="row" style={{display: 'grid', gridTemplateColumns: '1fr 1fr', gap: 12}}>
                <div>
                    <label className="label">Роль</label>
                    <Select
                        value={form.role}
                        onChange={(e: React.ChangeEvent<HTMLSelectElement>) =>
                            setForm({...form, role: e.currentTarget.value as Role})
                        }
                    >
                        <option value="ADMIN">ADMIN</option>
                        <option value="TEACHER">TEACHER</option>
                        <option value="STUDENT">STUDENT</option>
                    </Select>
                </div>
                {isEdit && (
                    <div>
                        <label className="label">Статус</label>
                        <Select
                            value={form.status}
                            onChange={(e: React.ChangeEvent<HTMLSelectElement>) =>
                                setForm({...form, status: e.currentTarget.value as Status})
                            }
                        >
                            <option value="ACTIVE">ACTIVE</option>
                            <option value="INACTIVE">INACTIVE</option>
                        </Select>
                    </div>
                )}
            </div>
            {!isEdit && (
                <div className="row">
                    <label className="label">Пароль (опционально)</label>
                    <Input
                        type="password"
                        value={form.password}
                        onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                            setForm({...form, password: e.currentTarget.value})
                        }
                        placeholder="если пусто — пароль сгенерируется"
                    />
                </div>
            )}
            {error && <div className="error">{error}</div>}
            <div style={{display: 'flex', justifyContent: 'flex-end', gap: 8, paddingTop: 8}}>
                <Button type="submit" className="primary" disabled={saving}>{submitLabel}</Button>
            </div>
        </form>
    );
}
