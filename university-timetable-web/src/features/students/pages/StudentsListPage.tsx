import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {studentsApi} from '@/api/students';
import type {CreateStudentRequest, StudentResponse, UpdateStudentRequest} from '@/types/students';
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

interface CsvRow {
    fullName: string;
    studentId: string;
    rowNumber: number;
}

type ImportTaskError = {
    rowNumber: number;
    message: string;
};

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

export const StudentsListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<StudentResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);
    const [fullName, setFullName] = useState('');
    const [studentId, setStudentId] = useState('');
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await studentsApi.getAll();
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
        setFullName('');
        setStudentId('');
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (s: StudentResponse) => {
        setFormMode('edit');
        setEditingId(s.id);
        setFullName(s.fullName);
        setStudentId(s.studentId);
        setFormError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!fullName.trim() || !studentId.trim()) {
            setFormError('Заполните ФИО и ID');
            return;
        }

        setSaving(true);
        setFormError(null);
        try {
            if (formMode === 'create') {
                const body: CreateStudentRequest = {
                    fullName: fullName.trim(),
                    studentId: studentId.trim(),
                };
                const created = await studentsApi.create(body);
                setItems(prev => [...prev, created]);
            } else if (formMode === 'edit' && editingId) {
                const body: UpdateStudentRequest = {
                    fullName: fullName.trim(),
                    studentId: studentId.trim(),
                };
                const updated = await studentsApi.update(editingId, body);
                setItems(prev => prev.map(s => (s.id === updated.id ? updated : s)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения студента');
        } finally {
            setSaving(false);
        }
    };

    const handleArchive = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await studentsApi.archive(id);
            setItems(prev => prev.map(s => (s.id === updated.id ? updated : s)));
        } catch (e) {
            console.error(e);
            alert('Не удалось архивировать студента');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await studentsApi.activate(id);
            setItems(prev => prev.map(s => (s.id === updated.id ? updated : s)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать студента');
        }
    };

    const parseCsv = (text: string): CsvRow[] => {
        const lines = text.trim().split(/\r?\n/);
        if (lines.length < 2) return [];

        const header = lines[0].split(',').map(h => h.trim());
        if (header.length !== 2 || header[0] !== 'fullName' || header[1] !== 'studentId') {
            throw new Error('Ожидаются колонки: fullName,studentId');
        }

        const rows: CsvRow[] = [];
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;
            const [fullName, studentId] = line.split(',');
            rows.push({
                fullName: (fullName ?? '').trim(),
                studentId: (studentId ?? '').trim(),
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
            const tasks: Array<() => Promise<StudentResponse>> = [];

            for (const row of rows) {
                if (!row.fullName || !row.studentId) {
                    validationErrors.push(`Строка ${row.rowNumber}: пустые fullName или studentId`);
                    continue;
                }

                tasks.push(async () => {
                    try {
                        return await studentsApi.create({
                            fullName: row.fullName,
                            studentId: row.studentId,
                        });
                    } catch (err: any) {
                        const msg =
                            err?.body?.message ??
                            (typeof err?.message === 'string' ? err.message : null) ??
                            'ошибка создания';
                        throw {rowNumber: row.rowNumber, message: msg};
                    }
                });
            }

            const {results: createdStudents, errors: taskErrors} = await runWithLimit(tasks, 8);

            if (createdStudents.length > 0) {
                setItems(prev => [...prev, ...createdStudents]);
            }

            const errors: string[] = [...validationErrors];
            for (const e of taskErrors) {
                errors.push(`Строка ${e.rowNumber}: ${e.message}`);
            }

            const summary = [
                `Успешно создано: ${createdStudents.length}`,
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
            'students.csv',
            ['fullName', 'studentId', 'status', 'createdAt', 'updatedAt'],
            items.map(s => [s.fullName, s.studentId, s.status, s.createdAt, s.updatedAt]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать студента"
            onCreate={openCreateForm}
        />
    );

    return (
        <Page title="Студенты" actions={actions}>
            <CsvMessages error={csvError} result={csvResult} />

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="ФИО">
                        <Input value={fullName} onChange={e => setFullName(e.target.value)} />
                    </FormField>
                    <FormField label="Студенческий ID">
                        <Input value={studentId} onChange={e => setStudentId(e.target.value)} />
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

            {loading && <p>Загрузка…</p>}

            {!loading && (
                <DataTable>
                    <thead>
                    <tr>
                        <th align="left">ID</th>
                        <th align="left">ФИО</th>
                        <th align="left">Статус</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(s => (
                        <tr key={s.id}>
                            <td>{s.studentId}</td>
                            <td>{s.fullName}</td>
                            <td>{s.status}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button variant="ghost" type="button" onClick={() => openEditForm(s)}>
                                        Редактировать
                                    </Button>
                                    {s.status === 'ACTIVE' ? (
                                        <Button variant="ghost" type="button" onClick={() => handleArchive(s.id)}>
                                            Архивировать
                                        </Button>
                                    ) : (
                                        <Button variant="ghost" type="button" onClick={() => handleActivate(s.id)}>
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 4 : 3}>Нет студентов</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
