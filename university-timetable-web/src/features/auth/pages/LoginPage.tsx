import React, {useState} from 'react';
import {useLocation, useNavigate} from 'react-router-dom';
import {useAuth} from '@/providers/AuthProvider';
import {Page} from '@/components/layout/Page';
import {Input} from '@/components/ui/Input';
import {FormField} from '@/components/ui/FormField';
import {Button} from '@/components/ui/Button';

interface LocationState {
    from?: Location;
}

export const LoginPage: React.FC = () => {
    const {login} = useAuth();
    const navigate = useNavigate();
    const location = useLocation();
    const state = location.state as LocationState | undefined;

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
            const from = state?.from?.pathname ?? '/';
            navigate(from, {replace: true});
        } catch (err) {
            console.error(err);
            setError('Неверный логин или пароль');
        } finally {
            setLoading(false);
        }
    };

    return (
        <Page title="Вход">
            <form onSubmit={handleSubmit}>
                <FormField label="Логин">
                    <Input
                        value={loginValue}
                        onChange={e => setLoginValue(e.target.value)}
                        autoComplete="username"
                    />
                </FormField>
                <FormField label="Пароль">
                    <Input
                        type="password"
                        value={password}
                        onChange={e => setPassword(e.target.value)}
                        autoComplete="current-password"
                    />
                </FormField>
                {error && <div style={{color: '#f97373', marginBottom: 8}}>{error}</div>}
                <Button type="submit" disabled={loading}>
                    {loading ? 'Входим…' : 'Войти'}
                </Button>
            </form>
        </Page>
    );
};
