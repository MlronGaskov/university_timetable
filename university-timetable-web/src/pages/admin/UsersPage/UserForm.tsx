import React, {useState} from 'react';
import type {UserResponse, Role, CreateUserRequest, UpdateUserRequest} from '@/types/auth';
import {usersApi} from '@/api/users';
import type {ApiErrorResponse} from '@/types/api-error';
import {FormErrorAlert} from '@/components/common/FormErrorAlert/FormErrorAlert';
import './UsersPage.css';
import {UUID} from "@/types/common";

interface Props {
    mode: 'create' | 'edit';
    initialUser?: UserResponse;
    onSaved: () => void;
    onCancel: () => void;
    onError: (err: unknown) => void;
    apiError: ApiErrorResponse | null;
}

export const UserForm: React.FC<Props> = ({
                                              mode,
                                              initialUser,
                                              onSaved,
                                              onCancel,
                                              onError,
                                              apiError,
                                          }) => {
    const [login, setLogin] = useState(initialUser?.login ?? '');
    const [email, setEmail] = useState(initialUser?.email ?? '');
    const [role, setRole] = useState<Role>(initialUser?.role ?? 'STUDENT');
    const [status, setStatus] = useState(initialUser?.status ?? 'ACTIVE');
    const [teacherId, setTeacherId] = useState<string>(initialUser?.teacherId ?? '');
    const [studentId, setStudentId] = useState<string>(initialUser?.studentId ?? '');
    const [newPassword, setNewPassword] = useState('');
    const [tempPassword, setTempPassword] = useState<string | null>(null);
    const [loading, setLoading] = useState(false);

    const title = mode === 'create' ? 'Новый пользователь' : 'Редактирование пользователя';

    const roleHelp =
        role === 'ADMIN'
            ? 'ADMIN не связывается с преподавателем или студентом.'
            : role === 'TEACHER'
                ? 'Для TEACHER укажи UUID преподавателя из каталога преподавателей.'
                : 'Для STUDENT укажи UUID студента из каталога студентов.';

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setTempPassword(null);
        setLoading(true);

        try {
            if (mode === 'create') {
                const basePayload: Partial<CreateUserRequest> = {
                    login,
                    email,
                    role,
                    teacherId: role === 'TEACHER' ? (teacherId || null) : null,
                    studentId: role === 'STUDENT' ? (studentId || null) : null,
                };

                const trimmedPassword = newPassword.trim();
                if (trimmedPassword) {
                    (basePayload as any).password = trimmedPassword;
                }
                const res: any = await usersApi.create(basePayload as CreateUserRequest);

                const generatedPassword: string | undefined =
                    res?.generatedPassword || res?.tempPassword || res?.password;

                if (generatedPassword) {
                    setTempPassword(generatedPassword);
                    onSaved();
                } else {
                    onSaved();
                    onCancel();
                }
            } else {
                if (!initialUser) return;

                const payload: UpdateUserRequest = {
                    email,
                    role,
                    status,
                    teacherId: role === 'TEACHER' ? (teacherId || null) : null,
                    studentId: role === 'STUDENT' ? (studentId || null) : null,
                };

                await usersApi.update(initialUser.id as UUID, payload);

                const trimmedPassword = newPassword.trim();
                if (trimmedPassword) {
                    await usersApi.setPassword(initialUser.id as UUID, {
                        newPassword: trimmedPassword,
                    });
                }

                onSaved();
                onCancel();
            }
        } catch (err) {
            onError(err);
        } finally {
            setLoading(false);
        }
    };

    return (
        <form className="UserForm-root" onSubmit={handleSubmit}>
            <div className="UserForm-header">
                <h2>{title}</h2>

                {mode === 'edit' && initialUser && (
                    <div className="UserForm-meta">
                        <span>
                            Создан: {new Date(initialUser.createdAt).toLocaleString()}
                        </span>
                        <span>
                            Обновлён: {new Date(initialUser.updatedAt).toLocaleString()}
                        </span>
                    </div>
                )}
            </div>

            {apiError && (
                <div style={{marginBottom: 8}}>
                    <FormErrorAlert apiError={apiError}/>
                </div>
            )}

            {mode === 'create' && (
                <div className="UserForm-field">
                    <span>Логин</span>
                    <input
                        type="text"
                        value={login}
                        onChange={e => setLogin(e.target.value)}
                        required
                    />
                </div>
            )}

            <div className="UserForm-field">
                <span>Email</span>
                <input
                    type="email"
                    value={email}
                    onChange={e => setEmail(e.target.value)}
                    required
                />
            </div>

            <div className="UserForm-field">
                <span>Роль</span>
                <select
                    value={role}
                    onChange={e => setRole(e.target.value as Role)}
                >
                    <option value="ADMIN">ADMIN</option>
                    <option value="TEACHER">TEACHER</option>
                    <option value="STUDENT">STUDENT</option>
                </select>
                <small>{roleHelp}</small>
            </div>

            {mode === 'edit' && (
                <div className="UserForm-field">
                    <span>Статус</span>
                    <select
                        value={status}
                        onChange={e => setStatus(e.target.value as UserResponse['status'])}
                    >
                        <option value="ACTIVE">ACTIVE</option>
                        <option value="INACTIVE">INACTIVE</option>
                    </select>
                </div>
            )}

            {role === 'TEACHER' && (
                <div className="UserForm-field">
                    <span>UUID преподавателя</span>
                    <input
                        type="text"
                        value={teacherId}
                        onChange={e => setTeacherId(e.target.value)}
                    />
                </div>
            )}

            {role === 'STUDENT' && (
                <div className="UserForm-field">
                    <span>UUID студента</span>
                    <input
                        type="text"
                        value={studentId}
                        onChange={e => setStudentId(e.target.value)}
                    />
                </div>
            )}

            <div className="UserForm-field">
                <span>{mode === 'create' ? 'Пароль (опционально)' : 'Новый пароль (опционально)'}</span>
                <input
                    type="password"
                    value={newPassword}
                    onChange={e => setNewPassword(e.target.value)}
                    placeholder={
                        mode === 'create'
                            ? 'Оставь пустым, чтобы сгенерировать автоматически'
                            : 'Оставь пустым, чтобы не менять пароль'
                    }
                />
            </div>

            {tempPassword && (
                <div className="UserForm-tempPassword">
                    <strong>Сгенерированный пароль:</strong>{' '}
                    <code>{tempPassword}</code>
                    <div style={{marginTop: 4, fontSize: 12}}>
                        Скопируй этот пароль — повторно он показан не будет.
                    </div>
                </div>
            )}

            <div className="FormActions">
                <button
                    type="button"
                    className="SecondaryButton"
                    onClick={onCancel}
                    disabled={loading}
                >
                    Закрыть
                </button>
                <button
                    type="submit"
                    className="PrimaryButton"
                    disabled={loading}
                >
                    {loading ? 'Сохранение...' : 'Сохранить'}
                </button>
            </div>
        </form>
    );
};
