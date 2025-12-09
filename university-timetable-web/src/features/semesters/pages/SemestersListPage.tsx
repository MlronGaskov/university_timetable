import React, {useEffect, useState} from 'react';
import {Link} from 'react-router-dom';
import {Page} from '@/components/layout/Page';
import {semestersApi} from '@/api/semesters';
import type {CreateSemesterRequest, SemesterResponse, UpdateSemesterRequest,} from '@/types/semesters';
import {formatDate, formatInstant} from '@/utils/formatters';
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
import styles from './SemestersListPage.module.css';

interface CsvRow {
    code: string;
    startAt: string;
    endAt: string;
    policyName: string;
    courseCodes: string[];
    roomCodes: string[];
    rowNumber: number;
}

const splitCsvLine = (line: string): string[] => {
    const result: string[] = [];
    let current = '';
    let inQuotes = false;

    for (let i = 0; i < line.length; i++) {
        const ch = line[i];

        if (ch === '"') {
            if (inQuotes && line[i + 1] === '"') {
                current += '"';
                i++;
            } else {
                inQuotes = !inQuotes;
            }
        } else if (ch === ',' && !inQuotes) {
            result.push(current);
            current = '';
        } else {
            current += ch;
        }
    }
    result.push(current);
    return result;
};

const toDateInputValue = (iso: string | null | undefined): string => {
    if (!iso) return '';
    const d = new Date(iso);
    if (Number.isNaN(d.getTime())) return '';
    return d.toISOString().slice(0, 10);
};

const parseIdsFromInput = (value: string): string[] =>
    value
        .split(/[;\n,]/)
        .map(v => v.trim())
        .filter(Boolean);

const formatIdsToInput = (ids: string[]): string => ids.join(';');

const dateToInstant = (date: string): string => {
    return new Date(date + 'T00:00:00Z').toISOString();
};

export const SemestersListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<SemesterResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);
    const [code, setCode] = useState('');
    const [startDate, setStartDate] = useState('');
    const [endDate, setEndDate] = useState('');
    const [policyName, setPolicyName] = useState('');
    const [courseCodesInput, setCourseCodesInput] = useState('');
    const [roomCodesInput, setRoomCodesInput] = useState('');
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await semestersApi.getAll();
                setItems(data);
            } catch (e) {
                console.error(e);
            } finally {
                setLoading(false);
            }
        })();
    }, []);

    const resetForm = () => {
        setFormMode(null);
        setEditingId(null);
        setCode('');
        setStartDate('');
        setEndDate('');
        setPolicyName('');
        setCourseCodesInput('');
        setRoomCodesInput('');
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (s: SemesterResponse) => {
        setFormMode('edit');
        setEditingId(s.id);
        setCode(s.code);
        setStartDate(toDateInputValue(s.startAt));
        setEndDate(toDateInputValue(s.endAt));
        setPolicyName(s.policyName);
        setCourseCodesInput(formatIdsToInput(s.courseCodes ?? []));
        setRoomCodesInput(formatIdsToInput(s.roomCodes ?? []));
        setFormError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!code.trim() || !startDate.trim() || !endDate.trim() || !policyName.trim()) {
            setFormError('Заполните код, даты начала и конца, а также имя политики');
            return;
        }

        const courseCodes = parseIdsFromInput(courseCodesInput);
        const roomCodes = parseIdsFromInput(roomCodesInput);

        const base: Omit<CreateSemesterRequest, 'code'> = {
            startAt: dateToInstant(startDate),
            endAt: dateToInstant(endDate),
            policyName: policyName.trim(),
            courseCodes,
            roomCodes,
        };

        setSaving(true);
        setFormError(null);

        try {
            if (formMode === 'create') {
                const body: CreateSemesterRequest = {
                    code: code.trim(),
                    ...base,
                };
                const created = await semestersApi.create(body);
                setItems(prev => [...prev, created]);
            } else if (formMode === 'edit' && editingId) {
                const body: UpdateSemesterRequest = {
                    code: code.trim(),
                    startAt: base.startAt,
                    endAt: base.endAt,
                    policyName: base.policyName,
                    courseCodes: base.courseCodes,
                    roomCodes: base.roomCodes,
                };
                const updated = await semestersApi.update(editingId, body);
                setItems(prev => prev.map(s => (s.id === updated.id ? updated : s)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения семестра');
        } finally {
            setSaving(false);
        }
    };

    const handleArchive = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await semestersApi.archive(id);
            setItems(prev => prev.map(s => (s.id === updated.id ? updated : s)));
        } catch (e) {
            console.error(e);
            alert('Не удалось архивировать семестр');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await semestersApi.activate(id);
            setItems(prev => prev.map(s => (s.id === updated.id ? updated : s)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать семестр');
        }
    };

    const parseCsv = (text: string): CsvRow[] => {
        const lines = text.trim().split(/\r?\n/);
        if (lines.length < 2) return [];

        const header = splitCsvLine(lines[0]).map(h => h.trim());
        const idx = (name: string) => header.indexOf(name);

        const idxCode = idx('code');
        const idxStartAt = idx('startAt');
        const idxEndAt = idx('endAt');
        const idxPolicyName = idx('policyName');
        const idxCourseCodes = idx('courseCodes');
        const idxRoomCodes = idx('roomCodes');

        if (
            idxCode < 0 ||
            idxStartAt < 0 ||
            idxEndAt < 0 ||
            idxPolicyName < 0
        ) {
            throw new Error(
                'Ожидаются колонки как минимум: code,startAt,endAt,policyName[,courseCodes,roomCodes]',
            );
        }

        const rows: CsvRow[] = [];
        for (let i = 1; i < lines.length; i++) {
            const rawLine = lines[i].trim();
            if (!rawLine) continue;
            const parts = splitCsvLine(rawLine);

            const safe = (idx: number) => (parts[idx] ?? '').trim();

            const courseCodes: string[] = [];
            if (idxCourseCodes >= 0) {
                const raw = safe(idxCourseCodes);
                if (raw) {
                    raw.split(';')
                        .map(v => v.trim())
                        .filter(Boolean)
                        .forEach(v => courseCodes.push(v));
                }
            }

            const roomCodes: string[] = [];
            if (idxRoomCodes >= 0) {
                const raw = safe(idxRoomCodes);
                if (raw) {
                    raw.split(';')
                        .map(v => v.trim())
                        .filter(Boolean)
                        .forEach(v => roomCodes.push(v));
                }
            }
            console.log(courseCodes, policyName);

            rows.push({
                code: safe(idxCode),
                startAt: safe(idxStartAt),
                endAt: safe(idxEndAt),
                policyName: safe(idxPolicyName),
                courseCodes,
                roomCodes,
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

            let success = 0;
            const errors: string[] = [];

            for (const row of rows) {
                if (!row.code || !row.startAt || !row.endAt || !row.policyName) {
                    errors.push(
                        `Строка ${row.rowNumber}: не заполнены code/startAt/endAt/policyName`,
                    );
                    continue;
                }

                try {
                    const body: CreateSemesterRequest = {
                        code: row.code,
                        startAt: row.startAt,
                        endAt: row.endAt,
                        policyName: row.policyName,
                        courseCodes: row.courseCodes,
                        roomCodes: row.roomCodes,
                    };
                    const created = await semestersApi.create(body);
                    success++;
                    setItems(prev => [...prev, created]);
                } catch (err: any) {
                    console.error(err);
                    errors.push(
                        `Строка ${row.rowNumber}: ${
                            err?.body?.message ?? 'ошибка создания семестра'
                        }`,
                    );
                }
            }

            const summary = [
                `Успешно создано семестров: ${success}`,
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
            'semesters.csv',
            [
                'code',
                'startAt',
                'endAt',
                'status',
                'policyName',
                'courseCodes',
                'roomCodes',
                'createdAt',
                'updatedAt',
            ],
            items.map(s => [
                s.code,
                s.startAt,
                s.endAt,
                s.status,
                s.policyName,
                (s.courseCodes ?? []).join(';'),
                (s.roomCodes ?? []).join(';'),
                s.createdAt,
                s.updatedAt,
            ]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать семестр"
            onCreate={openCreateForm}
        />
    );

    return (
        <Page title="Семестры" actions={actions}>
            <CsvMessages error={csvError} result={csvResult}/>

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Код">
                        <Input value={code} onChange={e => setCode(e.target.value)}/>
                    </FormField>

                    <FormField label="Начало (YYYY-MM-DD)">
                        <Input
                            type="date"
                            value={startDate}
                            onChange={e => setStartDate(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Конец (YYYY-MM-DD)">
                        <Input
                            type="date"
                            value={endDate}
                            onChange={e => setEndDate(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Имя политики">
                        <Input
                            value={policyName}
                            onChange={e => setPolicyName(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Коды курсов (через ; или перенос строки)">
                        <textarea
                            className={styles.idsTextarea}
                            value={courseCodesInput}
                            onChange={e => setCourseCodesInput(e.target.value)}
                            placeholder="MATH-101;CS-202"
                        />
                    </FormField>

                    <FormField label="Коды аудиторий (через ; или перенос строки)">
                        <textarea
                            className={styles.idsTextarea}
                            value={roomCodesInput}
                            onChange={e => setRoomCodesInput(e.target.value)}
                            placeholder="A-101;B-202;C-303"
                        />
                    </FormField>

                    <div className={crudStyles.formActions}>
                        <Button type="submit" disabled={saving}>
                            {saving
                                ? 'Сохранение…'
                                : formMode === 'create'
                                    ? 'Создать'
                                    : 'Сохранить'}
                        </Button>
                        <Button type="button" variant="ghost" onClick={resetForm}>
                            Отмена
                        </Button>
                    </div>

                    {formError && (
                        <div className={crudStyles.formError}>{formError}</div>
                    )}

                    <p className={styles.hint}>
                        Поле <code>policyName</code> должно совпадать с именем политики
                        (Policy.name). Курсы привязываются по их уникальному коду
                        <code> Course.code </code>.
                    </p>
                </form>
            )}

            {loading && <p>Загрузка…</p>}

            {!loading && (
                <DataTable>
                    <thead>
                    <tr>
                        <th align="left">Код</th>
                        <th align="left">Начало</th>
                        <th align="left">Конец</th>
                        <th align="left">Политика</th>
                        <th align="left">Курсов</th>
                        <th align="left">Аудиторий</th>
                        <th align="left">Статус</th>
                        <th align="left">Расписания</th>
                        <th align="left">Обновлено</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(s => (
                        <tr key={s.id}>
                            <td>{s.code}</td>
                            <td>{formatDate(s.startAt)}</td>
                            <td>{formatDate(s.endAt)}</td>
                            <td>{s.policyName}</td>
                            <td>{s.courseCodes?.length ?? 0}</td>
                            <td>{s.roomCodes?.length ?? 0}</td>
                            <td>{s.status}</td>
                            <td>
                                <Link to={`/semesters/${s.code}/schedules`}>Открыть</Link>
                            </td>
                            <td>{formatInstant(s.updatedAt)}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button
                                        variant="ghost"
                                        type="button"
                                        onClick={() => openEditForm(s)}
                                    >
                                        Редактировать
                                    </Button>
                                    {s.status === 'ACTIVE' ? (
                                        <Button
                                            variant="ghost"
                                            type="button"
                                            onClick={() => handleArchive(s.id)}
                                        >
                                            Архивировать
                                        </Button>
                                    ) : (
                                        <Button
                                            variant="ghost"
                                            type="button"
                                            onClick={() => handleActivate(s.id)}
                                        >
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 10 : 9}>Нет семестров</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
