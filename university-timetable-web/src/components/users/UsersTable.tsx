import React from 'react';
import {UserDto} from '@/types';
import Button from '../ui/Button';

export default function UsersTable({users, onEdit, onToggle}: {
    users: UserDto[];
    onEdit: (u: UserDto) => void;
    onToggle: (u: UserDto) => void;
}) {
    return (
        <div className="card" style={{overflow: 'auto'}}>
            <table className="table">
                <thead>
                <tr>
                    <th>Login</th>
                    <th>Email</th>
                    <th>Role</th>
                    <th>Status</th>
                    <th>Created</th>
                    <th>Updated</th>
                    <th>Действия</th>
                </tr>
                </thead>
                <tbody>
                {users.map(u => (
                    <tr key={u.id}>
                        <td style={{fontWeight: 600}}>{u.login}</td>
                        <td>{u.email}</td>
                        <td style={{textAlign: 'center'}}>{u.role}</td>
                        <td style={{textAlign: 'center'}}>{u.status}</td>
                        <td>{new Date(u.createdAt).toLocaleString()}</td>
                        <td>{new Date(u.updatedAt).toLocaleString()}</td>
                        <td style={{textAlign: 'center'}}>
                            <div style={{display: 'flex', gap: 8, justifyContent: 'center'}}>
                                <Button onClick={() => onEdit(u)}>Редактировать</Button>
                                <Button onClick={() => onToggle(u)} className={u.status === 'ACTIVE' ? 'warn' : 'ok'}>
                                    {u.status === 'ACTIVE' ? 'Деактивировать' : 'Активировать'}
                                </Button>
                            </div>
                        </td>
                    </tr>
                ))}
                </tbody>
            </table>
        </div>
    );
}
