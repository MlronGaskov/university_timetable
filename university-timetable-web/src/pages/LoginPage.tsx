import React, {useState} from 'react';
import Input from '../components/ui/Input';
import Button from '../components/ui/Button';
import {useAuth} from '@hooks/useAuth';

export default function LoginPage() {
    const {login} = useAuth();
    const [form, setForm] = useState({login: '', password: ''});
    const [error, setError] = useState<string | null>(null);
    const [loading, setLoading] = useState(false);

    const submit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError(null);
        setLoading(true);
        try {
            await login(form.login, form.password);
            location.replace('/');
        } catch {
            setError('Неверный логин или пароль');
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="center">
            <form onSubmit={submit} className="card form" style={{padding: 24}}>
                <h1 style={{fontSize: 24, fontWeight: 700, margin: '0 0 12px'}}>Вход</h1>
                <div className="row">
                    <label className="label">Логин</label>
                    <Input
                        value={form.login}
                        onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                            setForm({...form, login: e.currentTarget.value})
                        }
                    />
                </div>
                <div className="row">
                    <label className="label">Пароль</label>
                    <Input
                        type="password"
                        value={form.password}
                        onChange={(e: React.ChangeEvent<HTMLInputElement>) =>
                            setForm({...form, password: e.currentTarget.value})
                        }
                    />
                </div>
                {error && <div className="error">{error}</div>}
                <Button type="submit" disabled={loading} className="primary" style={{width: '100%', marginTop: 8}}>
                    Войти
                </Button>
                <p className="note">Демо: root / root</p>
            </form>
        </div>
    );
}
