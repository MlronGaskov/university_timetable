import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {teachersApi} from '@/api/teachers';
import type {
    CreateTeacherRequest,
    DayOfWeek,
    TeacherResponse,
    UpdateTeacherRequest,
    WorkingIntervalDto,
} from '@/types/teachers';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import {Input} from '@/components/ui/Input';
import {FormField} from '@/components/ui/FormField';
import {Button} from '@/components/ui/Button';
import {DataTable} from '@/components/ui/DataTable';
import {ActionsCell} from '@/components/ui/ActionsCell';
import {CsvMessages} from '@/components/ui/CsvMessages';
import {CsvToolbar} from '@/components/ui/CsvToolbar';
import {downloadCsv} from '@/utils/csv';
import crudStyles from '@/components/ui/CrudFormLayout.module.css';
import styles from './TeachersListPage.module.css';

interface CsvRow {
    teacherId: string;
    fullName: string;
    rowNumber: number;
}

type ImportTaskError = {rowNumber: number; message: string};

const runWithLimit = async <T,>(
    tasks: Array<() => Promise<T>>,
    limit: number,
): Promise<{results: T[]; errors: ImportTaskError[]}> => {
    const results: T[] = [];
    const errors: ImportTaskError[] = [];
    let i = 0;

    const workers = new Array(Math.max(1, limit)).fill(null).map(async () => {
        while (true) {
            const idx = i++;
            if (idx >= tasks.length) return;
            try {
                const r = await tasks[idx]();
                results.push(r);
            } catch (e: any) {
                const rowNumber = Number(e?.rowNumber ?? 0) || 0;
                const message = String(e?.message ?? e?.body?.message ?? 'ошибка импорта');
                errors.push({rowNumber, message});
            }
        }
    });

    await Promise.all(workers);
    return {results, errors};
};

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

export const TeachersListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<TeacherResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);
    const [teacherId, setTeacherId] = useState('');
    const [fullName, setFullName] = useState('');
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    const [hoursTeacherId, setHoursTeacherId] = useState<string | null>(null);
    const [hours, setHours] = useState<WorkingIntervalDto[]>([]);
    const [hoursSaving, setHoursSaving] = useState(false);
    const [hoursError, setHoursError] = useState<string | null>(null);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await teachersApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    const openWorkingHoursForm = (t: TeacherResponse) => {
        if (!isAdmin) return;
        setHoursTeacherId(t.id);
        setHoursError(null);

        const base = getDefaultWorkingHours();
        const initial = base.map(def => {
            const existing = t.preferredWorkingHours?.find(h => h.day === def.day);
            return existing ?? def;
        });
        setHours(initial);
    };

    const handleHoursChange = (day: DayOfWeek, field: 'startTime' | 'endTime', value: string) => {
        setHours(prev => prev.map(h => (h.day === day ? {...h, [field]: value} : h)));
    };

    const handleHoursSave: React.FormEventHandler = async e => {
        e.preventDefault();
        if (!hoursTeacherId || !isAdmin) return;

        setHoursSaving(true);
        setHoursError(null);
        try {
            const updated = await teachersApi.updateWorkingHours(hoursTeacherId, {
                preferredWorkingHours: hours,
            });
            setItems(prev => prev.map(t => (t.id === updated.id ? updated : t)));
            setHoursTeacherId(null);
        } catch (err) {
            console.error(err);
            setHoursError('Ошибка сохранения рабочих часов');
        } finally {
            setHoursSaving(false);
        }
    };

    const handleHoursCancel = () => {
        setHoursTeacherId(null);
        setHours([]);
        setHoursError(null);
    };

    const resetForm = () => {
        setFormMode(null);
        setEditingId(null);
        setTeacherId('');
        setFullName('');
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (t: TeacherResponse) => {
        setFormMode('edit');
        setEditingId(t.id);
        setTeacherId(t.teacherId);
        setFullName(t.fullName);
        setFormError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!teacherId.trim() || !fullName.trim()) {
            setFormError('Заполните ID и ФИО');
            return;
        }

        setSaving(true);
        setFormError(null);

        try {
            if (formMode === 'create') {
                const body: CreateTeacherRequest = {teacherId: teacherId.trim(), fullName: fullName.trim()};
                const created = await teachersApi.create(body);

                let finalTeacher = created;
                try {
                    finalTeacher = await teachersApi.updateWorkingHours(created.id, {
                        preferredWorkingHours: getDefaultWorkingHours(),
                    });
                } catch (e) {
                    console.error(e);
                }

                setItems(prev => [...prev, finalTeacher]);
            } else if (formMode === 'edit' && editingId) {
                const body: UpdateTeacherRequest = {teacherId: teacherId.trim(), fullName: fullName.trim()};
                const updated = await teachersApi.update(editingId, body);
                setItems(prev => prev.map(t => (t.id === updated.id ? updated : t)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения преподавателя');
        } finally {
            setSaving(false);
        }
    };

    const handleArchive = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await teachersApi.archive(id);
            setItems(prev => prev.map(t => (t.id === updated.id ? updated : t)));
        } catch (e) {
            console.error(e);
            alert('Не удалось архивировать преподавателя');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await teachersApi.activate(id);
            setItems(prev => prev.map(t => (t.id === updated.id ? updated : t)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать преподавателя');
        }
    };

    const parseCsv = (text: string): CsvRow[] => {
        const lines = text.trim().split(/\r?\n/);
        if (lines.length < 2) return [];

        const header = lines[0].split(',').map(h => h.trim());
        if (header.length !== 2 || header[0] !== 'teacherId' || header[1] !== 'fullName') {
            throw new Error('Ожидаются колонки: teacherId,fullName');
        }

        const rows: CsvRow[] = [];
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;
            const [teacherId, fullName] = line.split(',');
            rows.push({
                teacherId: (teacherId ?? '').trim(),
                fullName: (fullName ?? '').trim(),
                rowNumber: i + 1,
            });
        }
        return rows;
    };

    const importFromFile = async (file: File) => {
        if (!isAdmin) return;

        setCsvError(null);
        setCsvResult(null);
        setCsvProcessing(true);

        try {
            const text = await file.text();
            const rows = parseCsv(text);

            if (rows.length === 0) {
                setCsvResult('Файл пуст или не содержит данных.');
                return;
            }

            const validationErrors: string[] = [];
            const tasks: Array<() => Promise<TeacherResponse>> = [];

            for (const row of rows) {
                if (!row.teacherId || !row.fullName) {
                    validationErrors.push(`Строка ${row.rowNumber}: пустые teacherId или fullName`);
                    continue;
                }

                tasks.push(async () => {
                    try {
                        const created = await teachersApi.create({
                            teacherId: row.teacherId,
                            fullName: row.fullName,
                        });

                        try {
                            return await teachersApi.updateWorkingHours(created.id, {
                                preferredWorkingHours: getDefaultWorkingHours(),
                            });
                        } catch (e) {
                            console.error(e);
                            return created;
                        }
                    } catch (err: any) {
                        const msg =
                            err?.body?.message ??
                            (typeof err?.message === 'string' ? err.message : null) ??
                            'ошибка создания';
                        throw {rowNumber: row.rowNumber, message: msg};
                    }
                });
            }

            const {results: createdTeachers, errors: taskErrors} = await runWithLimit(tasks, 8);

            if (createdTeachers.length > 0) {
                setItems(prev => [...prev, ...createdTeachers]);
            }

            const errors: string[] = [...validationErrors];
            for (const e of taskErrors) {
                errors.push(`Строка ${e.rowNumber}: ${e.message}`);
            }

            const summary = [
                `Успешно создано: ${createdTeachers.length}`,
                errors.length ? `Ошибок: ${errors.length}` : '',
                errors.length ? `\n${errors.join('\n')}` : '',
            ]
                .filter(Boolean)
                .join('\n');

            setCsvResult(summary);
        } catch (err: any) {
            console.error(err);
            setCsvError(err.message || 'Ошибка чтения или парсинга CSV');
        } finally {
            setCsvProcessing(false);
        }
    };

    const exportCsv = () => {
        downloadCsv(
            'teachers.csv',
            ['teacherId', 'fullName', 'status', 'createdAt', 'updatedAt'],
            items.map(t => [t.teacherId, t.fullName, t.status, t.createdAt, t.updatedAt]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать преподавателя"
            onCreate={openCreateForm}
        />
    );

    return (
        <Page title="Преподаватели" actions={actions}>
            <CsvMessages error={csvError} result={csvResult} />

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Teacher ID">
                        <Input value={teacherId} onChange={e => setTeacherId(e.target.value)} />
                    </FormField>
                    <FormField label="ФИО">
                        <Input value={fullName} onChange={e => setFullName(e.target.value)} />
                    </FormField>

                    <div className={crudStyles.formActions}>
                        <Button type="submit" disabled={saving}>
                            {saving ? 'Сохранение…' : formMode === 'create' ? 'Создать' : 'Сохранить'}
                        </Button>
                        <Button type="button" variant="ghost" onClick={resetForm}>
                            Отмена
                        </Button>
                    </div>

                    {formError && <div className={crudStyles.formError}>{formError}</div>}
                </form>
            )}

            {hoursTeacherId && (
                <form className={styles.hoursForm} onSubmit={handleHoursSave}>
                    <div className={styles.hoursHeader}>
                        <strong>Рабочие часы преподавателя</strong>
                        <Button type="button" variant="ghost" onClick={handleHoursCancel}>
                            Закрыть
                        </Button>
                    </div>

                    {dayOptions.map(d => {
                        const val =
                            hours.find(h => h.day === d.value) ?? {day: d.value, startTime: '09:00', endTime: '22:00'};
                        return (
                            <div key={d.value} className={styles.hoursRow}>
                                <span>{d.label}</span>
                                <Input
                                    type="time"
                                    value={val.startTime}
                                    onChange={e => handleHoursChange(d.value, 'startTime', e.target.value)}
                                />
                                <Input
                                    type="time"
                                    value={val.endTime}
                                    onChange={e => handleHoursChange(d.value, 'endTime', e.target.value)}
                                />
                            </div>
                        );
                    })}

                    {hoursError && <p className={styles.hoursError}>{hoursError}</p>}

                    <div className={styles.hoursFooter}>
                        <Button type="submit" disabled={hoursSaving}>
                            {hoursSaving ? 'Сохранение…' : 'Сохранить'}
                        </Button>
                        <Button type="button" variant="ghost" onClick={handleHoursCancel}>
                            Отмена
                        </Button>
                    </div>

                    <p className={styles.hoursHint}>
                        По умолчанию задано время 09:00–22:00 с понедельника по субботу (воскресенье не учитывается).
                    </p>
                </form>
            )}

            {loading && <p>Загрузка…</p>}

            {!loading && (
                <DataTable>
                    <thead>
                    <tr>
                        <th align="left">ID</th>
                        <th align="left">ФИО</th>
                        <th align="left">Статус</th>
                        <th align="left">Часы работы</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(t => (
                        <tr key={t.id}>
                            <td>{t.teacherId}</td>
                            <td>{t.fullName}</td>
                            <td>{t.status}</td>
                            <td>{t.preferredWorkingHours && t.preferredWorkingHours.length > 0 ? 'Заданы' : '—'}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button variant="ghost" type="button" onClick={() => openEditForm(t)}>
                                        Редактировать
                                    </Button>
                                    <Button variant="ghost" type="button" onClick={() => openWorkingHoursForm(t)}>
                                        Рабочие часы
                                    </Button>
                                    {t.status === 'ACTIVE' ? (
                                        <Button variant="ghost" type="button" onClick={() => handleArchive(t.id)}>
                                            Архивировать
                                        </Button>
                                    ) : (
                                        <Button variant="ghost" type="button" onClick={() => handleActivate(t.id)}>
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 5 : 4}>Нет преподавателей</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
