import React, {useState} from 'react';
import {useNavigate, useLocation} from 'react-router-dom';
import {useAuth} from '@/hooks/useAuth';
import './LoginPage.css';

export const LoginPage: React.FC = () => {
    const {login} = useAuth();
    const navigate = useNavigate();
    const location = useLocation() as any;

    const [loginValue, setLoginValue] = useState('');
    const [password, setPassword] = useState('');
    const [error, setError] = useState<string | null>(null);
    const [loading, setLoading] = useState(false);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setError(null);
        setLoading(true);

        try {
            await login(loginValue, password);
            const from = location.state?.from?.pathname ?? '/';
            navigate(from, {replace: true});
        } catch {
            setError('Неверный логин или пароль');
        } finally {
            setLoading(false);
        }
    };

    return (
        <div className="LoginPage-root">
            <div className="LoginPage-card">
                <h1 className="LoginPage-title">Вход в систему</h1>
                <p className="LoginPage-text">
                    Введите учётные данные, выданные администратором.
                </p>

                <form className="LoginPage-form" onSubmit={handleSubmit}>
                    <div className="LoginPage-field">
                        <span>Логин</span>
                        <input
                            type="text"
                            value={loginValue}
                            onChange={e => setLoginValue(e.target.value)}
                            autoComplete="username"
                        />
                    </div>

                    <div className="LoginPage-field">
                        <span>Пароль</span>
                        <input
                            type="password"
                            value={password}
                            onChange={e => setPassword(e.target.value)}
                            autoComplete="current-password"
                        />
                    </div>

                    {error && <div className="LoginPage-error">{error}</div>}

                    <div className="LoginPage-actions">
                        <button
                            type="submit"
                            className="LoginPage-button"
                            disabled={loading}
                        >
                            {loading ? 'Входим...' : 'Войти'}
                        </button>
                    </div>
                </form>
            </div>
        </div>
    );
};
