import React, {useEffect, useState, useMemo} from 'react';
import {Page} from '@/components/layout/Page';
import {groupsApi} from '@/api/groups';
import {studentsApi} from '@/api/students';
import type {
    CreateGroupRequest,
    GroupResponse,
    UpdateGroupRequest,
} from '@/types/groups';
import type {StudentResponse} from '@/types/students';
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
import styles from './GroupsListPage.module.css';

interface CsvRow {
    name: string;
    code: string;
    size: number;
    studentIds: string[];
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

export const GroupsListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<GroupResponse[]>([]);
    const [allStudents, setAllStudents] = useState<StudentResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);
    const [name, setName] = useState('');
    const [code, setCode] = useState('');
    const [size, setSize] = useState('');
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    const [studentsGroupId, setStudentsGroupId] = useState<string | null>(null);
    const [studentToAddStudentId, setStudentToAddStudentId] = useState('');
    const [studentsError, setStudentsError] = useState<string | null>(null);
    const [studentsProcessing, setStudentsProcessing] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const [groups, students] = await Promise.all([
                    groupsApi.getAll(),
                    studentsApi.getAll(),
                ]);
                setItems(groups);
                setAllStudents(students);
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
        setName('');
        setCode('');
        setSize('');
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (g: GroupResponse) => {
        setFormMode('edit');
        setEditingId(g.id);
        setName(g.name);
        setCode(g.code);
        setSize(String(g.size));
        setFormError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!name.trim() || !code.trim() || !size.trim()) {
            setFormError('Заполните название, код и размер группы');
            return;
        }

        const parsedSize = Number.parseInt(size.trim(), 10);
        if (!Number.isFinite(parsedSize) || parsedSize <= 0) {
            setFormError('Размер группы должен быть положительным числом');
            return;
        }

        setSaving(true);
        setFormError(null);

        try {
            if (formMode === 'create') {
                const body: CreateGroupRequest = {
                    name: name.trim(),
                    code: code.trim(),
                    size: parsedSize,
                };
                const created = await groupsApi.create(body);
                setItems(prev => [...prev, created]);
            } else if (formMode === 'edit' && editingId) {
                const body: UpdateGroupRequest = {
                    name: name.trim(),
                    code: code.trim(),
                    size: parsedSize,
                };
                const updated = await groupsApi.update(editingId, body);
                setItems(prev => prev.map(g => (g.id === updated.id ? updated : g)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения группы');
        } finally {
            setSaving(false);
        }
    };

    const handleArchive = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await groupsApi.archive(id);
            setItems(prev => prev.map(g => (g.id === updated.id ? updated : g)));
        } catch (e) {
            console.error(e);
            alert('Не удалось архивировать группу');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await groupsApi.activate(id);
            setItems(prev => prev.map(g => (g.id === updated.id ? updated : g)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать группу');
        }
    };

    const currentGroup = useMemo(
        () => (studentsGroupId ? items.find(g => g.id === studentsGroupId) ?? null : null),
        [studentsGroupId, items],
    );

    const currentGroupStudents: StudentResponse[] = useMemo(() => {
        if (!currentGroup) return [];
        const ids = new Set(currentGroup.studentIds);
        return allStudents.filter(s => ids.has(s.studentId));
    }, [currentGroup, allStudents]);

    const availableStudents: StudentResponse[] = useMemo(() => {
        if (!currentGroup) return [];
        const ids = new Set(currentGroup.studentIds);
        return allStudents.filter(s => !ids.has(s.studentId));
    }, [currentGroup, allStudents]);

    const openStudentsForm = (g: GroupResponse) => {
        if (!isAdmin) return;
        setStudentsGroupId(g.id);
        setStudentsError(null);
        setStudentToAddStudentId('');
    };

    const closeStudentsForm = () => {
        setStudentsGroupId(null);
        setStudentsError(null);
        setStudentToAddStudentId('');
    };

    const handleAddStudent: React.FormEventHandler = async e => {
        e.preventDefault();
        if (!isAdmin || !studentsGroupId || !studentToAddStudentId) return;

        setStudentsProcessing(true);
        setStudentsError(null);
        try {
            const updated = await groupsApi.addStudent(studentsGroupId, studentToAddStudentId);
            setItems(prev => prev.map(g => (g.id === updated.id ? updated : g)));
            setStudentToAddStudentId('');
        } catch (err) {
            console.error(err);
            setStudentsError('Не удалось добавить студента в группу');
        } finally {
            setStudentsProcessing(false);
        }
    };

    const handleRemoveStudent = async (studentId: string) => {
        if (!isAdmin || !studentsGroupId) return;

        setStudentsProcessing(true);
        setStudentsError(null);
        try {
            const updated = await groupsApi.removeStudent(studentsGroupId, studentId);
            setItems(prev => prev.map(g => (g.id === updated.id ? updated : g)));
        } catch (err) {
            console.error(err);
            setStudentsError('Не удалось удалить студента из группы');
        } finally {
            setStudentsProcessing(false);
        }
    };

    const parseCsv = (text: string): CsvRow[] => {
        const lines = text.trim().split(/\r?\n/);
        if (lines.length < 2) return [];

        const header = lines[0].split(',').map(h => h.trim());
        if (header.length < 3 || header[0] !== 'name' || header[1] !== 'code' || header[2] !== 'size') {
            throw new Error('Ожидаются колонки как минимум: name,code,size[,studentIds]');
        }

        const idxStudentIds = header.indexOf('studentIds');

        const rows: CsvRow[] = [];
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;
            const parts = line.split(',');

            const name = (parts[0] ?? '').trim();
            const code = (parts[1] ?? '').trim();
            const sizeStr = (parts[2] ?? '').trim();
            const parsedSize = Number.parseInt(sizeStr, 10);

            let studentIds: string[] = [];
            if (idxStudentIds >= 0) {
                const raw = (parts[idxStudentIds] ?? '').trim();
                if (raw) {
                    studentIds = raw.split(';').map(s => s.trim()).filter(Boolean);
                }
            }

            rows.push({
                name,
                code,
                size: parsedSize,
                studentIds,
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
            const tasks: Array<() => Promise<GroupResponse>> = [];

            for (const row of rows) {
                if (!row.name || !row.code || !Number.isFinite(row.size) || row.size <= 0) {
                    validationErrors.push(
                        `Строка ${row.rowNumber}: не заполнены name/code или некорректный size`,
                    );
                    continue;
                }

                tasks.push(async () => {
                    try {
                        const created = await groupsApi.create({
                            name: row.name,
                            code: row.code,
                            size: row.size,
                        });

                        // добавление студентов — внутри задачи (параллельно между группами)
                        let current = created;
                        if (row.studentIds && row.studentIds.length > 0) {
                            for (const sid of row.studentIds) {
                                current = await groupsApi.addStudent(current.id, sid);
                            }
                        }

                        return current;
                    } catch (err: any) {
                        const msg =
                            err?.body?.message ??
                            (typeof err?.message === 'string' ? err.message : null) ??
                            'ошибка создания группы';
                        throw {rowNumber: row.rowNumber, message: msg};
                    }
                });
            }

            const {results: createdGroups, errors: taskErrors} = await runWithLimit(tasks, 8);

            if (createdGroups.length > 0) {
                setItems(prev => [...prev, ...createdGroups]);
            }

            const errors: string[] = [...validationErrors];
            for (const e of taskErrors) {
                if (e.rowNumber) errors.push(`Строка ${e.rowNumber}: ${e.message}`);
                else errors.push(e.message);
            }

            const summary = [
                `Успешно создано групп: ${createdGroups.length}`,
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
            'groups.csv',
            ['name', 'code', 'size', 'status', 'createdAt', 'updatedAt', 'studentIds'],
            items.map(g => [
                g.name,
                g.code,
                g.size,
                g.status,
                g.createdAt,
                g.updatedAt,
                g.studentIds.join(';'),
            ]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать группу"
            onCreate={openCreateForm}
        />
    );

    return (
        <Page title="Группы" actions={actions}>
            <CsvMessages error={csvError} result={csvResult} />

            {isAdmin && studentsGroupId && currentGroup && (
                <form className={styles.studentsForm} onSubmit={handleAddStudent}>
                    <div className={styles.studentsHeader}>
                        <strong>Студенты группы {currentGroup.code}</strong>
                        <Button type="button" variant="ghost" onClick={closeStudentsForm}>
                            Закрыть
                        </Button>
                    </div>

                    <ul className={styles.studentsList}>
                        {currentGroupStudents.length === 0 && (
                            <li className={styles.empty}>В группе пока нет студентов.</li>
                        )}
                        {currentGroupStudents.map(s => (
                            <li key={s.id} className={styles.studentsRow}>
                                <span>
                                    {s.fullName}{' '}
                                    <span className={styles.studentId}>({s.studentId})</span>
                                </span>
                                <Button
                                    type="button"
                                    variant="ghost"
                                    onClick={() => handleRemoveStudent(s.studentId)}
                                    disabled={studentsProcessing}
                                >
                                    Удалить
                                </Button>
                            </li>
                        ))}
                    </ul>

                    <div className={styles.selectorRow}>
                        <select
                            className={styles.studentsSelect}
                            value={studentToAddStudentId}
                            onChange={e => setStudentToAddStudentId(e.target.value)}
                        >
                            <option value="">Выберите студента…</option>
                            {availableStudents.map(s => (
                                <option key={s.id} value={s.studentId}>
                                    {s.fullName} ({s.studentId})
                                </option>
                            ))}
                        </select>
                        <Button type="submit" disabled={!studentToAddStudentId || studentsProcessing}>
                            Добавить в группу
                        </Button>
                    </div>

                    {studentsError && <p className={styles.studentsError}>{studentsError}</p>}

                    <p className={styles.studentsHint}>
                        Список возможных студентов берётся из справочника «Студенты». Для CSV-экспорта
                        используется колонка <code>studentIds</code> — список полей{' '}
                        <code>studentId</code> студентов, разделённых точкой с запятой.
                    </p>
                </form>
            )}

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Название группы">
                        <Input value={name} onChange={e => setName(e.target.value)} />
                    </FormField>
                    <FormField label="Код группы">
                        <Input value={code} onChange={e => setCode(e.target.value)} />
                    </FormField>
                    <FormField label="Размер (кол-во мест)">
                        <Input type="number" min={1} value={size} onChange={e => setSize(e.target.value)} />
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
                        <th align="left">Код</th>
                        <th align="left">Название</th>
                        <th align="left">Размер</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(g => (
                        <tr key={g.id}>
                            <td>{g.code}</td>
                            <td>{g.name}</td>
                            <td>{g.size}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button variant="ghost" type="button" onClick={() => openEditForm(g)}>
                                        Редактировать
                                    </Button>
                                    <Button variant="ghost" type="button" onClick={() => openStudentsForm(g)}>
                                        Студенты
                                    </Button>
                                    {g.status === 'ACTIVE' ? (
                                        <Button variant="ghost" type="button" onClick={() => handleArchive(g.id)}>
                                            Архивировать
                                        </Button>
                                    ) : (
                                        <Button variant="ghost" type="button" onClick={() => handleActivate(g.id)}>
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 4 : 3}>Нет групп</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
