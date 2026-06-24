import React, {useEffect, useMemo, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {notificationsApi} from '@/api/notifications';
import type {NotificationEntryDto} from '@/types/notifications';
import {EmptyState} from '@/components/ui/EmptyState';
import styles from './NotificationsPage.module.css';

const REASON_LABELS: Record<string, string> = {
    TEACHER_WORKING_HOURS_CHANGED: 'Изменены рабочие часы преподавателя',
    MANUAL: 'Ручная перегенерация',
    INITIAL: 'Первичная генерация',
};

function formatTimestamp(ts: string): string {
    try {
        return new Date(ts).toLocaleString('ru-RU', {
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
            hour: '2-digit',
            minute: '2-digit',
        });
    } catch {
        return ts;
    }
}

export const NotificationsPage: React.FC = () => {
    const [entries, setEntries] = useState<NotificationEntryDto[]>([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);
    const [query, setQuery] = useState('');
    const [typeFilter, setTypeFilter] = useState<'all' | 'teacher' | 'student'>('all');

    useEffect(() => {
        (async () => {
            setLoading(true);
            setError(null);
            try {
                const data = await notificationsApi.getAll();
                setEntries(data);
            } catch (e) {
                console.error(e);
                setError('Не удалось загрузить уведомления');
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    const filtered = useMemo(() => {
        const q = query.trim().toLowerCase();
        return entries
            .map(entry => {
                const matchingUsers = entry.users.filter(u => {
                    if (typeFilter !== 'all' && u.userType !== typeFilter) return false;
                    if (!q) return true;
                    return (
                        u.fullName.toLowerCase().includes(q) ||
                        u.userId.toLowerCase().includes(q) ||
                        u.email.toLowerCase().includes(q)
                    );
                });
                return {...entry, users: matchingUsers};
            })
            .filter(entry => entry.users.length > 0);
    }, [entries, query, typeFilter]);

    return (
        <Page title="Уведомления об изменениях расписания">
            <div className={styles.filtersBar}>
                <input
                    className={styles.searchInput}
                    type="text"
                    placeholder="Поиск по имени, ID или email..."
                    value={query}
                    onChange={e => setQuery(e.target.value)}
                />
                <select
                    className={styles.select}
                    value={typeFilter}
                    onChange={e => setTypeFilter(e.target.value as typeof typeFilter)}
                >
                    <option value="all">Все</option>
                    <option value="teacher">Преподаватели</option>
                    <option value="student">Студенты</option>
                </select>
                {!loading && (
                    <span className={styles.hint}>
                        Событий: <b>{filtered.length}</b>
                    </span>
                )}
            </div>

            {loading && <p>Загрузка…</p>}
            {error && <p className={styles.error}>{error}</p>}

            {!loading && !error && filtered.length === 0 && (
                <EmptyState
                    icon="🔔"
                    title="Уведомлений нет"
                    description={
                        query || typeFilter !== 'all'
                            ? 'Ничего не найдено по выбранным фильтрам.'
                            : 'Уведомления появятся здесь после изменения расписания.'
                    }
                />
            )}

            {!loading && filtered.length > 0 && (
                <div className={styles.list}>
                    {filtered.map((entry, entryIdx) => (
                        <section key={entryIdx} className={styles.entryCard}>
                            <header className={styles.entryHeader}>
                                <span className={styles.entryTime}>{formatTimestamp(entry.timestamp)}</span>
                                {entry.semesterCode && (
                                    <code className={styles.semesterBadge}>{entry.semesterCode}</code>
                                )}
                                {entry.reason && (
                                    <span className={styles.reasonBadge}>
                                        {REASON_LABELS[entry.reason] ?? entry.reason}
                                    </span>
                                )}
                            </header>

                            <div className={styles.usersBlock}>
                                {entry.users.map((user, userIdx) => (
                                    <div key={userIdx} className={styles.userRow}>
                                        <div className={styles.userHeader}>
                                            <span className={user.userType === 'teacher' ? styles.badgeTeacher : styles.badgeStudent}>
                                                {user.userType === 'teacher' ? 'Преподаватель' : 'Студент'}
                                            </span>
                                            <span className={styles.userName}>{user.fullName}</span>
                                            <code className={styles.userId}>[{user.userId}]</code>
                                            <span className={styles.userEmail}>{user.email}</span>
                                        </div>
                                        <ul className={styles.changesList}>
                                            {user.changes.map((ch, chIdx) => (
                                                <li key={chIdx} className={ch.action === 'added' ? styles.added : styles.removed}>
                                                    <span className={styles.changeAction}>
                                                        {ch.action === 'added' ? '+' : '−'}
                                                    </span>
                                                    <span className={styles.changeDesc}>{ch.description}</span>
                                                </li>
                                            ))}
                                        </ul>
                                    </div>
                                ))}
                            </div>
                        </section>
                    ))}
                </div>
            )}
        </Page>
    );
};
