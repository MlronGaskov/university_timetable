import React, {useEffect, useMemo, useState} from 'react';
import {Link, useParams} from 'react-router-dom';
import {Page} from '@/components/layout/Page';
import {schedulesApi} from '@/api/schedules';
import type {ScheduleResponse} from '@/types/schedules';

const dayOrder: Record<string, number> = {
    MONDAY: 1,
    TUESDAY: 2,
    WEDNESDAY: 3,
    THURSDAY: 4,
    FRIDAY: 5,
    SATURDAY: 6,
    SUNDAY: 7,
};

const dayLabel: Record<string, string> = {
    MONDAY: 'Пн',
    TUESDAY: 'Вт',
    WEDNESDAY: 'Ср',
    THURSDAY: 'Чт',
    FRIDAY: 'Пт',
    SATURDAY: 'Сб',
    SUNDAY: 'Вс',
};

export const ScheduleDetailsPage: React.FC = () => {
    const {scheduleId} = useParams<'scheduleId'>();
    const [schedule, setSchedule] = useState<ScheduleResponse | null>(null);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState<string | null>(null);

    useEffect(() => {
        if (!scheduleId) return;

        (async () => {
            setLoading(true);
            setError(null);
            try {
                const data = await schedulesApi.getById(scheduleId);
                setSchedule(data);
            } catch (e) {
                console.error(e);
                setError('Не удалось загрузить расписание');
            } finally {
                setLoading(false);
            }
        })();
    }, [scheduleId]);

    const sortedSlots = useMemo(() => {
        if (!schedule) return [];
        return [...schedule.slots].sort((a, b) => {
            const da = dayOrder[a.dayOfWeek] ?? 99;
            const db = dayOrder[b.dayOfWeek] ?? 99;
            if (da !== db) return da - db;
            if (a.startTime < b.startTime) return -1;
            if (a.startTime > b.startTime) return 1;
            return 0;
        });
    }, [schedule]);

    const title = schedule
        ? `Расписание (версия ${schedule.version})`
        : 'Расписание';

    return (
        <Page title={title}>
            {schedule && (
                <p style={{marginTop: 0, marginBottom: 16, fontSize: 13}}>
                    Семестр ID: <code>{schedule.semesterId}</code> | Версия: {schedule.version} | Оценка:{' '}
                    {schedule.evaluationScore ?? '—'}
                    {' | '}
                    <Link to={`/semesters/${schedule.semesterId}/schedules`}>
                        Все версии семестра
                    </Link>
                </p>
            )}

            {loading && <p>Загрузка…</p>}
            {error && (
                <p style={{color: '#f97373', marginBottom: 8}}>
                    {error}
                </p>
            )}

            {!loading && schedule && sortedSlots.length === 0 && !error && (
                <p>В этой версии расписания нет слотов.</p>
            )}

            {!loading && schedule && sortedSlots.length > 0 && (
                <table style={{width: '100%', fontSize: 14, borderCollapse: 'collapse'}}>
                    <thead>
                    <tr>
                        <th align="left">День</th>
                        <th align="left">Время</th>
                        <th align="left">Курс ID</th>
                        <th align="left">Аудитория</th>
                        <th align="left">Период действия</th>
                        <th align="left">Шаблон недель</th>
                    </tr>
                    </thead>
                    <tbody>
                    {sortedSlots.map(slot => (
                        <tr key={slot.id}>
                            <td>{dayLabel[slot.dayOfWeek] ?? slot.dayOfWeek}</td>
                            <td>
                                {slot.startTime.slice(0, 5)}–{slot.endTime.slice(0, 5)}
                            </td>
                            <td>{slot.courseId}</td>
                            <td>{slot.roomCode}</td>
                            <td>
                                {slot.validFrom} — {slot.validUntil}
                            </td>
                            <td>
                                {slot.weekPattern === 'EVERY_WEEK' && 'Каждую неделю'}
                                {slot.weekPattern === 'ODD_WEEKS' && 'Нечётные недели'}
                                {slot.weekPattern === 'EVEN_WEEKS' && 'Чётные недели'}
                            </td>
                        </tr>
                    ))}
                    </tbody>
                </table>
            )}
        </Page>
    );
};
