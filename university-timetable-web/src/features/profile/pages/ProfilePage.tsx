import React, {useEffect, useMemo, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {useAuth} from '@/providers/AuthProvider';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import {teachersApi} from '@/api/teachers';
import type {DayOfWeek, TeacherResponse, WorkingIntervalDto} from '@/types/teachers';
import {Input} from '@/components/ui/Input';
import {Button} from '@/components/ui/Button';
import styles from './ProfilePage.module.css';

const dayOptions: { value: DayOfWeek; label: string }[] = [
    {value: 'MONDAY', label: 'Понедельник'},
    {value: 'TUESDAY', label: 'Вторник'},
    {value: 'WEDNESDAY', label: 'Среда'},
    {value: 'THURSDAY', label: 'Четверг'},
    {value: 'FRIDAY', label: 'Пятница'},
    {value: 'SATURDAY', label: 'Суббота'},
];

const getDefaultWorkingHours = (): WorkingIntervalDto[] => [
    {day: 'MONDAY', startTime: '09:00', endTime: '22:00'},
    {day: 'TUESDAY', startTime: '09:00', endTime: '22:00'},
    {day: 'WEDNESDAY', startTime: '09:00', endTime: '22:00'},
    {day: 'THURSDAY', startTime: '09:00', endTime: '22:00'},
    {day: 'FRIDAY', startTime: '09:00', endTime: '22:00'},
    {day: 'SATURDAY', startTime: '09:00', endTime: '22:00'},
];

export const ProfilePage: React.FC = () => {
    const {userId, teacherId, studentId} = useAuth();
    const {role, isAdmin, isTeacher, isStudent} = useRoleGuard();

    const [teacher, setTeacher] = useState<TeacherResponse | null>(null);
    const [teacherLoading, setTeacherLoading] = useState(false);
    const [teacherError, setTeacherError] = useState<string | null>(null);

    const [hoursFormOpen, setHoursFormOpen] = useState(false);
    const [hours, setHours] = useState<WorkingIntervalDto[]>([]);
    const [hoursSaving, setHoursSaving] = useState(false);
    const [hoursError, setHoursError] = useState<string | null>(null);

    useEffect(() => {
        if (!isTeacher || !teacherId) return;

        (async () => {
            setTeacherLoading(true);
            setTeacherError(null);
            try {
                const all = await teachersApi.getAll();
                const found =
                    all.find(t => t.teacherId === teacherId) ||
                    all.find(t => t.id === teacherId);
                setTeacher(found ?? null);
                if (!found) {
                    setTeacherError(
                        'Для вашей учётной записи не найден преподаватель в справочнике. ' +
                        'Попросите администратора добавить вас в список преподавателей.',
                    );
                }
            } catch (e) {
                console.error(e);
                setTeacherError('Не удалось загрузить данные преподавателя');
            } finally {
                setTeacherLoading(false);
            }
        })();
    }, [isTeacher, teacherId]);

    const startEditHours = () => {
        if (!teacher) return;
        const base = getDefaultWorkingHours();
        const current = teacher.preferredWorkingHours ?? [];
        const initial = base.map(def => {
            const existing = current.find(h => h.day === def.day);
            return existing ?? def;
        });

        setHours(initial);
        setHoursError(null);
        setHoursFormOpen(true);
    };

    const handleHoursChange = (day: DayOfWeek, field: 'startTime' | 'endTime', value: string) => {
        setHours(prev =>
            prev.map(h => (h.day === day ? {...h, [field]: value} : h)),
        );
    };

    const handleHoursSave: React.FormEventHandler = async e => {
        e.preventDefault();
        if (!teacher) return;

        setHoursSaving(true);
        setHoursError(null);
        try {
            const updated = await teachersApi.updateWorkingHours(teacher.id, {
                preferredWorkingHours: hours,
            });
            setTeacher(updated);
            setHoursFormOpen(false);
        } catch (err) {
            console.error(err);
            setHoursError('Ошибка сохранения рабочих часов');
        } finally {
            setHoursSaving(false);
        }
    };

    const handleHoursCancel = () => {
        setHoursFormOpen(false);
        setHoursError(null);
    };

    const teacherHoursSummary = useMemo(() => {
        if (!teacher || !teacher.preferredWorkingHours || teacher.preferredWorkingHours.length === 0) {
            return null;
        }
        const map = new Map<DayOfWeek, WorkingIntervalDto>();
        teacher.preferredWorkingHours.forEach(h => map.set(h.day, h));
        return dayOptions
            .filter(d => map.has(d.value))
            .map(d => {
                const h = map.get(d.value)!;
                return `${d.label}: ${h.startTime}–${h.endTime}`;
            });
    }, [teacher]);

    return (
        <Page title="Профиль">
            <div className={styles.grid}>
                <section className={styles.card}>
                    <h2 className={styles.sectionTitle}>Основная информация</h2>
                    <dl className={styles.infoList}>
                        <div>
                            <dt>Роль</dt>
                            <dd><b>{role ?? '—'}</b></dd>
                        </div>
                        <div>
                            <dt>User ID</dt>
                            <dd><code>{userId ?? '—'}</code></dd>
                        </div>
                        <div>
                            <dt>Teacher ID</dt>
                            <dd><code>{teacherId ?? '—'}</code></dd>
                        </div>
                        <div>
                            <dt>Student ID</dt>
                            <dd><code>{studentId ?? '—'}</code></dd>
                        </div>
                    </dl>
                    <p className={styles.muted}>
                        Эти данные приходят из токена авторизации и используются для настройки доступа в системе.
                    </p>
                </section>

                {isStudent && (
                    <section className={styles.card}>
                        <h2 className={styles.sectionTitle}>Информация студента</h2>
                        <p className={styles.muted}>
                            Ваш идентификатор студента: <code>{studentId ?? '—'}</code>.
                        </p>
                        <p className={styles.muted}>
                            Здесь можно позже вывести привязанные группы и курсы (например, на основе
                            <code> groupsApi </code> и <code>coursesApi</code>).
                        </p>
                    </section>
                )}

                {isAdmin && (
                    <section className={styles.card}>
                        <h2 className={styles.sectionTitle}>Администрирование</h2>
                        <ul className={styles.list}>
                            <li>Управление пользователями: раздел «Пользователи».</li>
                            <li>Управление справочниками: курсы, группы, аудитории, семестры, политики.</li>
                            <li>
                                Генерация расписания: страница семестра &rarr; «Сгенерировать расписание» и просмотр
                                версий.
                            </li>
                        </ul>
                    </section>
                )}

                {isTeacher && (
                    <section className={styles.card}>
                        <h2 className={styles.sectionTitle}>Настройки преподавателя</h2>

                        {teacherLoading && <p>Загрузка данных преподавателя…</p>}

                        {!teacherLoading && teacherError && (
                            <p className={styles.error}>{teacherError}</p>
                        )}

                        {!teacherLoading && teacher && (
                            <>
                                <p className={styles.teacherHeader}>
                                    <span className={styles.teacherName}>{teacher.fullName}</span>
                                    <span className={styles.badge}>ID: {teacher.teacherId}</span>
                                </p>
                                <p className={styles.muted}>
                                    Эти предпочтительные часы используются при генерации расписания как
                                    «желательные» временные окна.
                                </p>

                                {teacherHoursSummary && teacherHoursSummary.length > 0 ? (
                                    <ul className={styles.hoursSummary}>
                                        {teacherHoursSummary.map(line => (
                                            <li key={line}>{line}</li>
                                        ))}
                                    </ul>
                                ) : (
                                    <p className={styles.muted}>
                                        Рабочие часы пока не заданы. Вы можете указать, в какие дни и время вам удобно
                                        вести занятия.
                                    </p>
                                )}

                                <Button
                                    variant="secondary"
                                    type="button"
                                    onClick={startEditHours}
                                >
                                    {teacher?.preferredWorkingHours?.length ? 'Изменить часы работы' : 'Задать часы работы'}
                                </Button>

                                {hoursFormOpen && (
                                    <form className={styles.hoursForm} onSubmit={handleHoursSave}>
                                        <div className={styles.hoursFormHeader}>
                                            <strong>Желаемые часы работы</strong>
                                            <Button
                                                type="button"
                                                variant="ghost"
                                                onClick={handleHoursCancel}
                                            >
                                                Отмена
                                            </Button>
                                        </div>

                                        {dayOptions.map(d => {
                                            const val =
                                                hours.find(h => h.day === d.value) ?? {
                                                    day: d.value,
                                                    startTime: '09:00',
                                                    endTime: '22:00',
                                                };
                                            return (
                                                <div key={d.value} className={styles.hoursRow}>
                                                    <span className={styles.hoursDay}>{d.label}</span>
                                                    <Input
                                                        type="time"
                                                        value={val.startTime}
                                                        onChange={e =>
                                                            handleHoursChange(d.value, 'startTime', e.target.value)
                                                        }
                                                    />
                                                    <Input
                                                        type="time"
                                                        value={val.endTime}
                                                        onChange={e =>
                                                            handleHoursChange(d.value, 'endTime', e.target.value)
                                                        }
                                                    />
                                                </div>
                                            );
                                        })}

                                        {hoursError && (
                                            <p className={styles.error}>{hoursError}</p>
                                        )}

                                        <div className={styles.hoursFooter}>
                                            <Button type="submit" disabled={hoursSaving}>
                                                {hoursSaving ? 'Сохранение…' : 'Сохранить'}
                                            </Button>
                                            <Button
                                                type="button"
                                                variant="ghost"
                                                onClick={handleHoursCancel}
                                            >
                                                Отмена
                                            </Button>
                                        </div>

                                        <p className={styles.hint}>
                                            По умолчанию задаётся интервал 09:00–22:00 с понедельника по субботу.
                                            Воскресенье обычно не используется для занятий.
                                        </p>
                                    </form>
                                )}
                            </>
                        )}
                    </section>
                )}
            </div>
        </Page>
    );
};
