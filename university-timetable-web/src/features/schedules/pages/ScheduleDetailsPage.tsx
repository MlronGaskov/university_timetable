import React, {useEffect, useMemo, useState} from 'react';
import {Link, useParams} from 'react-router-dom';
import {Page} from '@/components/layout/Page';
import {schedulesApi} from '@/api/schedules';
import {coursesApi} from '@/api/courses';
import {teachersApi} from '@/api/teachers';
import {groupsApi} from '@/api/groups';
import {roomsApi} from '@/api/rooms';
import type {ScheduleResponse} from '@/types/schedules';
import type {CourseResponse} from '@/types/courses';
import type {TeacherResponse} from '@/types/teachers';
import type {GroupResponse} from '@/types/groups';
import type {RoomResponse} from '@/types/rooms';
import {downloadCsv} from '@/utils/csv';
import {Button} from '@/components/ui/Button';
import styles from './ScheduleDetailsPage.module.css';

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

type ViewMode = 'ALL' | 'TEACHER' | 'GROUP';

interface EnrichedSlot {
    id: string;
    dayOfWeek: string;
    startTime: string;
    endTime: string;
    weekPattern: string;
    validFrom: string;
    validUntil: string;
    course: CourseResponse | undefined;
    teacher: TeacherResponse | undefined;
    groups: GroupResponse[];
    room: RoomResponse | undefined;
}

export const ScheduleDetailsPage: React.FC = () => {
    const {scheduleId} = useParams<'scheduleId'>();

    const [schedule, setSchedule] = useState<ScheduleResponse | null>(null);
    const [loadingSchedule, setLoadingSchedule] = useState(false);
    const [loadingMeta, setLoadingMeta] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const [courses, setCourses] = useState<CourseResponse[]>([]);
    const [teachers, setTeachers] = useState<TeacherResponse[]>([]);
    const [groups, setGroups] = useState<GroupResponse[]>([]);
    const [rooms, setRooms] = useState<RoomResponse[]>([]);

    const [viewMode, setViewMode] = useState<ViewMode>('ALL');
    const [selectedTeacherId, setSelectedTeacherId] = useState<string>('');
    const [selectedGroupId, setSelectedGroupId] = useState<string>('');

    useEffect(() => {
        if (!scheduleId) return;

        (async () => {
            setLoadingSchedule(true);
            setError(null);
            try {
                const data = await schedulesApi.getById(scheduleId);
                setSchedule(data);
            } catch (e) {
                console.error(e);
                setError('Не удалось загрузить расписание');
            } finally {
                setLoadingSchedule(false);
            }
        })();
    }, [scheduleId]);

    useEffect(() => {
        if (!schedule) return;

        (async () => {
            setLoadingMeta(true);
            try {
                const [allCourses, allTeachers, allGroups, allRooms] = await Promise.all([
                    coursesApi.getAll(),
                    teachersApi.getAll(),
                    groupsApi.getAll(),
                    roomsApi.getAll(),
                ]);

                const courseCodes = new Set(schedule.slots.map(s => s.courseCode));
                const filteredCourses = allCourses.filter(c => courseCodes.has(c.code));

                setCourses(filteredCourses);
                setTeachers(allTeachers);
                setGroups(allGroups);
                setRooms(allRooms);
            } catch (e) {
                console.error(e);
            } finally {
                setLoadingMeta(false);
            }
        })();
    }, [schedule]);

    const enrichedSlots: EnrichedSlot[] = useMemo(() => {
        if (!schedule) return [];

        return schedule.slots.map(slot => {
            const course = courses.find(c => c.code === slot.courseCode);
            const teacher = course
                ? teachers.find(t => t.teacherId === course.teacherId)
                : undefined;

            const courseGroups: GroupResponse[] = course
                ? groups.filter(g => course.groupCodes.includes(g.id))
                : [];

            const room = rooms.find(r => r.roomCode === slot.roomCode);

            return {
                id: slot.id,
                dayOfWeek: slot.dayOfWeek,
                startTime: slot.startTime,
                endTime: slot.endTime,
                weekPattern: slot.weekPattern,
                validFrom: slot.validFrom,
                validUntil: slot.validUntil,
                course,
                teacher,
                groups: courseGroups,
                room,
            };
        });
    }, [schedule, courses, teachers, groups, rooms]);

    const teacherOptions = useMemo(() => {
        const ids = new Set<string>();
        const result: TeacherResponse[] = [];

        for (const s of enrichedSlots) {
            if (s.teacher && !ids.has(s.teacher.id)) {
                ids.add(s.teacher.id);
                result.push(s.teacher);
            }
        }
        return result.sort((a, b) => a.fullName.localeCompare(b.fullName));
    }, [enrichedSlots]);

    const groupOptions = useMemo(() => {
        const ids = new Set<string>();
        const result: GroupResponse[] = [];

        for (const s of enrichedSlots) {
            for (const g of s.groups) {
                if (!ids.has(g.id)) {
                    ids.add(g.id);
                    result.push(g);
                }
            }
        }
        return result.sort((a, b) => a.name.localeCompare(b.name));
    }, [enrichedSlots]);

    const filteredSlots = useMemo(() => {
        let slots = enrichedSlots;

        if (viewMode === 'TEACHER' && selectedTeacherId) {
            slots = slots.filter(
                s => s.teacher && s.teacher.id === selectedTeacherId,
            );
        }

        if (viewMode === 'GROUP' && selectedGroupId) {
            slots = slots.filter(s =>
                s.groups.some(g => g.id === selectedGroupId),
            );
        }

        return [...slots].sort((a, b) => {
            const da = dayOrder[a.dayOfWeek] ?? 99;
            const db = dayOrder[b.dayOfWeek] ?? 99;
            if (da !== db) return da - db;
            if (a.startTime < b.startTime) return -1;
            if (a.startTime > b.startTime) return 1;
            return 0;
        });
    }, [enrichedSlots, viewMode, selectedTeacherId, selectedGroupId]);

    const slotsByDay = useMemo(() => {
        const map = new Map<string, EnrichedSlot[]>();
        for (const slot of filteredSlots) {
            if (!map.has(slot.dayOfWeek)) {
                map.set(slot.dayOfWeek, []);
            }
            map.get(slot.dayOfWeek)!.push(slot);
        }

        for (const [key, arr] of map.entries()) {
            arr.sort((a, b) => (a.startTime < b.startTime ? -1 : 1));
            map.set(key, arr);
        }

        return Array.from(map.entries()).sort(
            ([a], [b]) => (dayOrder[a] ?? 99) - (dayOrder[b] ?? 99),
        );
    }, [filteredSlots]);

    const handleExportCsv = () => {
        if (!schedule) return;

        downloadCsv(
            `schedule_${schedule.version}.csv`,
            [
                'dayOfWeek',
                'dayLabel',
                'startTime',
                'endTime',
                'weekPattern',
                'validFrom',
                'validUntil',
                'courseCode',
                'courseTitle',
                'teacher',
                'groups',
                'roomCode',
                'roomBuilding',
                'roomNumber',
            ],
            filteredSlots.map(s => [
                s.dayOfWeek,
                dayLabel[s.dayOfWeek] ?? s.dayOfWeek,
                s.startTime,
                s.endTime,
                s.weekPattern,
                s.validFrom,
                s.validUntil,
                s.course?.code ?? '',
                s.course?.title ?? '',
                s.teacher?.fullName ?? s.teacher?.teacherId ?? '',
                s.groups.map(g => g.code).join(';'),
                s.room?.roomCode ?? '',
                s.room?.building ?? '',
                s.room?.number ?? '',
            ]),
        );
    };

    const title = schedule
        ? `Расписание (версия ${schedule.version})`
        : 'Расписание';

    const isLoading = loadingSchedule || (schedule && loadingMeta);

    return (
        <Page
            title={title}
            actions={
                schedule ? (
                    <Button
                        type="button"
                        variant="secondary"
                        onClick={handleExportCsv}
                        disabled={filteredSlots.length === 0}
                    >
                        Экспорт CSV
                    </Button>
                ) : null
            }
        >
            {schedule && (
                <p className={styles.infoLine}>
                    Семестр: <code>{schedule.semesterCode}</code> | Версия:{' '}
                    {schedule.version} | Оценка:{' '}
                    {schedule.evaluationScore ?? '—'} |{' '}
                    <Link to={`/semesters/${schedule.semesterCode}/schedules`}>
                        Все версии семестра
                    </Link>
                </p>
            )}

            {isLoading && <p>Загрузка расписания…</p>}

            {error && (
                <p className={styles.error}>
                    {error}
                </p>
            )}

            {!isLoading && schedule && !error && (
                <>
                    <div className={styles.filtersBar}>
                        <div className={styles.modeBlock}>
                            <span className={styles.modeLabel}>Режим:</span>
                            <select
                                className={styles.select}
                                value={viewMode}
                                onChange={e => {
                                    const v = e.target.value as ViewMode;
                                    setViewMode(v);
                                    setSelectedTeacherId('');
                                    setSelectedGroupId('');
                                }}
                            >
                                <option value="ALL">Все слоты</option>
                                <option value="TEACHER">По преподавателю</option>
                                <option value="GROUP">По группе</option>
                            </select>
                        </div>

                        {viewMode === 'TEACHER' && (
                            <div className={styles.modeBlock}>
                                <span className={styles.modeLabel}>Преподаватель:</span>
                                <select
                                    className={styles.select}
                                    value={selectedTeacherId}
                                    onChange={e => setSelectedTeacherId(e.target.value)}
                                >
                                    <option value="">Все</option>
                                    {teacherOptions.map(t => (
                                        <option key={t.id} value={t.id}>
                                            {t.fullName} ({t.teacherId})
                                        </option>
                                    ))}
                                </select>
                            </div>
                        )}

                        {viewMode === 'GROUP' && (
                            <div className={styles.modeBlock}>
                                <span className={styles.modeLabel}>Группа:</span>
                                <select
                                    className={styles.select}
                                    value={selectedGroupId}
                                    onChange={e => setSelectedGroupId(e.target.value)}
                                >
                                    <option value="">Все</option>
                                    {groupOptions.map(g => (
                                        <option key={g.id} value={g.id}>
                                            {g.name} ({g.code})
                                        </option>
                                    ))}
                                </select>
                            </div>
                        )}
                    </div>

                    {filteredSlots.length === 0 && (
                        <p>Для выбранных фильтров нет слотов.</p>
                    )}

                    {filteredSlots.length > 0 && (
                        <div className={styles.daysGrid}>
                            {slotsByDay.map(([day, slots]) => (
                                <section key={day} className={styles.dayCard}>
                                    <header className={styles.dayHeader}>
                                        <span className={styles.dayLabel}>
                                            {dayLabel[day] ?? day}
                                        </span>
                                        <span className={styles.daySubLabel}>
                                            {day}
                                        </span>
                                    </header>
                                    <ul className={styles.slotsList}>
                                        {slots.map(s => (
                                            <li key={s.id} className={styles.slotRow}>
                                                <div className={styles.slotTime}>
                                                    {s.startTime.slice(0, 5)}–{s.endTime.slice(0, 5)}
                                                    {s.weekPattern === 'ODD_WEEKS' && (
                                                        <span className={styles.weekTag}>
                                                            нечётные
                                                        </span>
                                                    )}
                                                    {s.weekPattern === 'EVEN_WEEKS' && (
                                                        <span className={styles.weekTag}>
                                                            чётные
                                                        </span>
                                                    )}
                                                </div>
                                                <div className={styles.slotMain}>
                                                    <div className={styles.slotTitle}>
                                                        {s.course?.code ?? s.course?.id ?? 'Курс'}
                                                        {s.course?.title && (
                                                            <span className={styles.slotTitleSecondary}>
                                                                {' — '}{s.course.title}
                                                            </span>
                                                        )}
                                                    </div>
                                                    {s.teacher && (
                                                        <div className={styles.slotLine}>
                                                            <span className={styles.label}>
                                                                Преподаватель:
                                                            </span>
                                                            <span>
                                                                {s.teacher.fullName}{' '}
                                                                <span className={styles.muted}>
                                                                    ({s.teacher.teacherId})
                                                                </span>
                                                            </span>
                                                        </div>
                                                    )}
                                                    {s.groups.length > 0 && (
                                                        <div className={styles.slotLine}>
                                                            <span className={styles.label}>Группы:</span>
                                                            <span>
                                                                {s.groups
                                                                    .map(g => `${g.name} (${g.code})`)
                                                                    .join(', ')}
                                                            </span>
                                                        </div>
                                                    )}
                                                    <div className={styles.slotLine}>
                                                        <span className={styles.label}>Аудитория:</span>
                                                        <span>
                                                            {s.room?.roomCode ?? s.room?.id ?? s.room?.roomCode ?? '—'}
                                                            {s.room && (
                                                                <span className={styles.muted}>
                                                                    {` — ${s.room.building} / ${s.room.number}`}
                                                                </span>
                                                            )}
                                                        </span>
                                                    </div>
                                                    <div className={styles.slotLine}>
                                                        <span className={styles.label}>Период:</span>
                                                        <span className={styles.muted}>
                                                            {s.validFrom} — {s.validUntil}
                                                        </span>
                                                    </div>
                                                </div>
                                            </li>
                                        ))}
                                    </ul>
                                </section>
                            ))}
                        </div>
                    )}
                </>
            )}
        </Page>
    );
};
