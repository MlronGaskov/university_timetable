import React, {useCallback, useEffect, useMemo, useState} from 'react';
import {Link, useParams} from 'react-router-dom';
import {Page} from '@/components/layout/Page';
import {schedulesApi} from '@/api/schedules';
import {semestersApi} from '@/api/semesters';
import type {ScheduleSummaryResponse} from '@/types/schedules';
import type {SemesterResponse} from '@/types/semesters';
import {formatDate, formatInstant} from '@/utils/formatters';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import {Button} from '@/components/ui/Button';
import styles from './SemesterSchedulesPage.module.css';

export const SemesterSchedulesPage: React.FC = () => {
    const {semesterCode} = useParams<{ semesterCode: string }>();
    const {isAdmin} = useRoleGuard();

    const [semester, setSemester] = useState<SemesterResponse | null>(null);
    const [schedules, setSchedules] = useState<ScheduleSummaryResponse[]>([]);
    const [loading, setLoading] = useState(false);
    const [generating, setGenerating] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const normalizedSemesterCode = useMemo(
        () => (semesterCode ?? '').trim(),
        [semesterCode],
    );

    const load = useCallback(async () => {
        if (!normalizedSemesterCode) return;

        setLoading(true);
        setError(null);
        try {
            const [allSemesters, scheds] = await Promise.all([
                semestersApi.getAll(),
                schedulesApi.getForSemester(normalizedSemesterCode),
            ]);

            const sem =
                allSemesters.find(
                    s => s.code.toLowerCase() === normalizedSemesterCode.toLowerCase(),
                ) ?? null;

            setSemester(sem);
            setSchedules(scheds);
        } catch (e) {
            console.error(e);
            setError('Не удалось загрузить расписания для семестра');
        } finally {
            setLoading(false);
        }
    }, [normalizedSemesterCode]);

    useEffect(() => {
        if (!normalizedSemesterCode) return;
        void load();
    }, [load, normalizedSemesterCode]);

    const handleGenerate = useCallback(async () => {
        if (!normalizedSemesterCode) return;

        setGenerating(true);
        setError(null);
        try {
            const newSchedule = await schedulesApi.generateForSemester(normalizedSemesterCode);

            setSchedules(prev => {
                const filtered = prev.filter(s => s.id !== newSchedule.id);
                return [newSchedule, ...filtered];
            });
        } catch (e) {
            console.error(e);
            setError('Ошибка генерации расписания');
        } finally {
            setGenerating(false);
        }
    }, [normalizedSemesterCode]);

    const title = semester
        ? `Расписания семестра ${semester.code}`
        : normalizedSemesterCode
            ? `Расписания семестра ${normalizedSemesterCode}`
            : 'Расписания семестра';

    const isEmpty = !loading && !error && schedules.length === 0;
    return (
        <Page
            title={title}
            actions={
                isAdmin && normalizedSemesterCode ? (
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
                <p className={styles.infoLine}>
                    Период: {formatDate(semester.startAt)} — {formatDate(semester.endAt)} | Политика:{' '}
                    <code>{semester.policyName}</code>
                </p>
            )}

            {loading && <p>Загрузка…</p>}
            {error && (
                <p className={styles.error}>
                    {error}
                </p>
            )}

            {isEmpty && (
                <p>Для этого семестра ещё нет сгенерированных расписаний.</p>
            )}

            {!loading && schedules.length > 0 && (
                <div className={styles.grid}>
                    {schedules.map(s => (
                        <section key={s.id} className={styles.card}>
                            <header className={styles.cardHeader}>
                                <div className={styles.cardTitle}>
                                    Версия <span className={styles.version}>{s.version}</span>
                                </div>
                                <span className={styles.scoreTag}>
                                    оценка: {s.evaluationScore ?? '—'}
                                </span>
                            </header>

                            <div className={styles.cardBody}>
                                <div className={styles.line}>
                                    <span className={styles.label}>Создано:</span>
                                    <span className={styles.muted}>{formatInstant(s.createdAt)}</span>
                                </div>
                                {/* УБРАЛИ "Обновлено", как просил */}
                            </div>

                            <footer className={styles.cardFooter}>
                                <Link className={styles.linkButton} to={`/schedules/${s.id}`}>
                                    Открыть
                                </Link>
                            </footer>
                        </section>
                    ))}
                </div>
            )}
        </Page>
    );
};
