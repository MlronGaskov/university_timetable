import React, {useEffect, useState} from 'react';
import {Link, useParams} from 'react-router-dom';
import {Page} from '@/components/layout/Page';
import {schedulesApi} from '@/api/schedules';
import {semestersApi} from '@/api/semesters';
import type {ScheduleSummaryResponse} from '@/types/schedules';
import type {SemesterResponse} from '@/types/semesters';
import {formatDate, formatInstant} from '@/utils/formatters';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import {Button} from '@/components/ui/Button';

export const SemesterSchedulesPage: React.FC = () => {
    const {semesterId} = useParams<'semesterId'>();

    const {isAdmin} = useRoleGuard();

    const [semester, setSemester] = useState<SemesterResponse | null>(null);
    const [schedules, setSchedules] = useState<ScheduleSummaryResponse[]>([]);
    const [loading, setLoading] = useState(false);
    const [generating, setGenerating] = useState(false);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        if (!semesterId) return;

        (async () => {
            setLoading(true);
            setError(null);
            try {
                const [sem, scheds] = await Promise.all([
                    semestersApi.getById(semesterId),
                    schedulesApi.getForSemester(semesterId),
                ]);
                setSemester(sem);
                setSchedules(scheds);
            } catch (e) {
                console.error(e);
                setError('Не удалось загрузить расписания для семестра');
            } finally {
                setLoading(false);
            }
        })();
    }, [semesterId]);

    const handleGenerate = async () => {
        if (!semesterId) return;
        setGenerating(true);
        setError(null);
        try {
            const newSchedule = await schedulesApi.generateForSemester(semesterId);
            // Добавляем новую версию в начало списка
            setSchedules(prev => [newSchedule, ...prev]);
        } catch (e) {
            console.error(e);
            setError('Ошибка генерации расписания');
        } finally {
            setGenerating(false);
        }
    };

    const title = semester
        ? `Расписания семестра ${semester.code}`
        : 'Расписания семестра';

    return (
        <Page
            title={title}
            actions={
                isAdmin && semesterId ? (
                    <Button
                        variant="primary"
                        onClick={handleGenerate}
                        disabled={generating}
                    >
                        {generating ? 'Генерация…' : 'Сгенерировать расписание'}
                    </Button>
                ) : null
            }
        >
            {semester && (
                <p style={{marginTop: 0, marginBottom: 16, fontSize: 13}}>
                    Период: {formatDate(semester.startAt)} — {formatDate(semester.endAt)} | Политика:{' '}
                    <code>{semester.policyId}</code>
                </p>
            )}

            {loading && <p>Загрузка…</p>}
            {error && (
                <p style={{color: '#f97373', marginBottom: 8}}>
                    {error}
                </p>
            )}

            {!loading && schedules.length === 0 && !error && (
                <p>Для этого семестра ещё нет сгенерированных расписаний.</p>
            )}

            {!loading && schedules.length > 0 && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">Версия</th>
                        <th align="left">Оценка</th>
                        <th align="left">Создано</th>
                        <th align="left">Обновлено</th>
                        <th align="left">Действия</th>
                    </tr>
                    </thead>
                    <tbody>
                    {schedules.map(s => (
                        <tr key={s.id}>
                            <td>{s.version}</td>
                            <td>{s.evaluationScore ?? '—'}</td>
                            <td>{formatInstant(s.createdAt)}</td>
                            <td>{formatInstant(s.updatedAt)}</td>
                            <td>
                                <Link to={`/schedules/${s.id}`}>Открыть</Link>
                            </td>
                        </tr>
                    ))}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
