import React, {useEffect, useMemo, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {coursesApi} from '@/api/courses';
import {groupsApi} from '@/api/groups';
import type {
    CourseEquipmentItemDto,
    CourseResponse,
    CreateCourseRequest,
    UpdateCourseRequest,
} from '@/types/courses';
import type {GroupResponse} from '@/types/groups';
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
import styles from './CoursesListPage.module.css';

interface CsvRow {
    code: string;
    title: string;
    teacherId: string;
    plannedHours: number;
    equipmentJson: string | null;
    groupCodes: string[];
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

const parseCsv = (text: string): CsvRow[] => {
    const lines = text.trim().split(/\r?\n/);
    if (lines.length < 2) return [];

    const header = splitCsvLine(lines[0]).map(h => h.trim());
    const idx = (name: string) => header.indexOf(name);

    const idxCode = idx('code');
    const idxTitle = idx('title');
    const idxTeacherId = idx('teacherId');
    const idxPlannedHours = idx('plannedHours');
    const idxEquipmentJson = idx('equipmentJson');
    const idxGroupCodes = idx('groupCodes');

    if (idxCode < 0 || idxTitle < 0 || idxTeacherId < 0 || idxPlannedHours < 0) {
        throw new Error(
            'Ожидаются колонки как минимум: code,title,teacherId,plannedHours[,equipmentJson,groupCodes]',
        );
    }

    const rows: CsvRow[] = [];
    for (let i = 1; i < lines.length; i++) {
        const rawLine = lines[i].trim();
        if (!rawLine) continue;
        const parts = splitCsvLine(rawLine);

        const safe = (idxN: number) => (parts[idxN] ?? '').trim();

        const hoursStr = safe(idxPlannedHours);
        const hours = Number.parseInt(hoursStr, 10);

        let groupCodes: string[] = [];
        if (idxGroupCodes >= 0) {
            const raw = safe(idxGroupCodes);
            if (raw) {
                groupCodes = raw
                    .split(';')
                    .map(s => s.trim())
                    .filter(Boolean);
            }
        }

        rows.push({
            code: safe(idxCode),
            title: safe(idxTitle),
            teacherId: safe(idxTeacherId),
            plannedHours: hours,
            equipmentJson: idxEquipmentJson >= 0 ? safe(idxEquipmentJson) || null : null,
            groupCodes,
            rowNumber: i + 1,
        });
    }
    return rows;
};

const runWithLimit = async <T,>(
    tasks: Array<() => Promise<T>>,
    limit: number,
): Promise<{results: T[]; errors: any[]}> => {
    const results: T[] = [];
    const errors: any[] = [];

    let i = 0;
    const workers = new Array(Math.max(1, limit)).fill(null).map(async () => {
        while (true) {
            const idx = i++;
            if (idx >= tasks.length) return;
            try {
                const r = await tasks[idx]();
                results.push(r);
            } catch (e) {
                errors.push(e);
            }
        }
    });

    await Promise.all(workers);
    return {results, errors};
};

export const CoursesListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<CourseResponse[]>([]);
    const [groups, setGroups] = useState<GroupResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);
    const [code, setCode] = useState('');
    const [title, setTitle] = useState('');
    const [teacherId, setTeacherId] = useState('');
    const [plannedHours, setPlannedHours] = useState('');
    const [equipment, setEquipment] = useState<CourseEquipmentItemDto[]>([]);
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    const [groupsCourseId, setGroupsCourseId] = useState<string | null>(null);
    const [groupToAddCode, setGroupToAddCode] = useState('');
    const [groupsError, setGroupsError] = useState<string | null>(null);
    const [groupsProcessing, setGroupsProcessing] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const [coursesData, groupsData] = await Promise.all([
                    coursesApi.getAll(),
                    groupsApi.getAll(),
                ]);
                setItems(coursesData);
                setGroups(groupsData);
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
        setTitle('');
        setTeacherId('');
        setPlannedHours('');
        setEquipment([]);
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (c: CourseResponse) => {
        setFormMode('edit');
        setEditingId(c.id);
        setCode(c.code);
        setTitle(c.title);
        setTeacherId(c.teacherId);
        setPlannedHours(String(c.plannedHours));
        setEquipment(c.equipmentRequirements ?? []);
        setFormError(null);
    };

    const handleEquipmentChange = (index: number, field: 'name' | 'quantity', value: string) => {
        setEquipment(prev =>
            prev.map((it, i) => {
                if (i !== index) return it;
                if (field === 'name') return {...it, name: value};
                const q = Number.parseInt(value, 10);
                return {...it, quantity: Number.isFinite(q) && q > 0 ? q : 0};
            }),
        );
    };

    const handleAddEquipment = () => {
        setEquipment(prev => [...prev, {name: '', quantity: 1}]);
    };

    const handleRemoveEquipment = (index: number) => {
        setEquipment(prev => prev.filter((_, i) => i !== index));
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!code.trim() || !title.trim() || !teacherId.trim() || !plannedHours.trim()) {
            setFormError('Заполните код, название, преподавателя и часы');
            return;
        }

        const hours = Number.parseInt(plannedHours.trim(), 10);
        if (!Number.isFinite(hours) || hours <= 0) {
            setFormError('Часы должны быть положительным числом');
            return;
        }

        const normalizedEquipment: CourseEquipmentItemDto[] = (equipment ?? [])
            .map(it => ({
                name: String(it.name ?? '').trim(),
                quantity: Number.parseInt(String(it.quantity ?? '0'), 10),
            }))
            .filter(it => it.name && Number.isFinite(it.quantity) && it.quantity > 0);

        setSaving(true);
        setFormError(null);

        try {
            if (formMode === 'create') {
                const body: CreateCourseRequest = {
                    code: code.trim(),
                    title: title.trim(),
                    teacherId: teacherId.trim(),
                    plannedHours: hours,
                    equipmentRequirements: normalizedEquipment,
                    groupCodes: [],
                };
                const created = await coursesApi.create(body);
                setItems(prev => [...prev, created]);
            } else if (formMode === 'edit' && editingId) {
                const body: UpdateCourseRequest = {
                    code: code.trim(),
                    title: title.trim(),
                    teacherId: teacherId.trim(),
                    plannedHours: hours,
                    equipmentRequirements: normalizedEquipment,
                };
                const updated = await coursesApi.update(editingId, body);
                setItems(prev => prev.map(c => (c.id === updated.id ? updated : c)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения курса');
        } finally {
            setSaving(false);
        }
    };

    const handleArchive = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await coursesApi.archive(id);
            setItems(prev => prev.map(c => (c.id === updated.id ? updated : c)));
        } catch (e) {
            console.error(e);
            alert('Не удалось архивировать курс');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await coursesApi.activate(id);
            setItems(prev => prev.map(c => (c.id === updated.id ? updated : c)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать курс');
        }
    };

    const equipmentTitle = (eq: CourseEquipmentItemDto[]): string => {
        if (!eq || eq.length === 0) return '—';
        return eq.map(i => `${i.name}×${i.quantity}`).join('\n');
    };

    const renderEquipment = (eq: CourseEquipmentItemDto[]) => {
        if (!eq || eq.length === 0) return '—';
        return (
            <div title={equipmentTitle(eq)}>
                {eq.map((i, idx) => (
                    <React.Fragment key={`${i.name}-${idx}`}>
                        {i.name}×{i.quantity}
                        {idx < eq.length - 1 ? <br /> : null}
                    </React.Fragment>
                ))}
            </div>
        );
    };

    const currentCourse = useMemo(
        () => (groupsCourseId ? items.find(c => c.id === groupsCourseId) ?? null : null),
        [groupsCourseId, items],
    );

    const currentCourseGroups: GroupResponse[] = useMemo(() => {
        if (!currentCourse) return [];
        const codes = new Set(currentCourse.groupCodes);
        return groups.filter(g => codes.has(g.code));
    }, [currentCourse, groups]);

    const availableGroups: GroupResponse[] = useMemo(() => {
        if (!currentCourse) return [];
        const codes = new Set(currentCourse.groupCodes);
        return groups.filter(g => !codes.has(g.code));
    }, [currentCourse, groups]);

    const openGroupsForm = (c: CourseResponse) => {
        if (!isAdmin) return;
        setGroupsCourseId(c.id);
        setGroupToAddCode('');
        setGroupsError(null);
    };

    const closeGroupsForm = () => {
        setGroupsCourseId(null);
        setGroupToAddCode('');
        setGroupsError(null);
    };

    const handleAddGroup: React.FormEventHandler = async e => {
        e.preventDefault();
        if (!isAdmin || !groupsCourseId || !groupToAddCode) return;

        setGroupsProcessing(true);
        setGroupsError(null);
        try {
            const updated = await coursesApi.addGroup(groupsCourseId, groupToAddCode);
            setItems(prev => prev.map(c => (c.id === updated.id ? updated : c)));
            setGroupToAddCode('');
        } catch (err) {
            console.error(err);
            setGroupsError('Не удалось добавить группу к курсу');
        } finally {
            setGroupsProcessing(false);
        }
    };

    const handleRemoveGroup = async (groupCode: string) => {
        if (!isAdmin || !groupsCourseId) return;

        setGroupsProcessing(true);
        setGroupsError(null);
        try {
            const updated = await coursesApi.removeGroup(groupsCourseId, groupCode);
            setItems(prev => prev.map(c => (c.id === updated.id ? updated : c)));
        } catch (err) {
            console.error(err);
            setGroupsError('Не удалось удалить группу из курса');
        } finally {
            setGroupsProcessing(false);
        }
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

            let createdCount = 0;
            const errors: string[] = [];
            const createdCourses: CourseResponse[] = [];

            for (const row of rows) {
                if (
                    !row.code ||
                    !row.title ||
                    !row.teacherId ||
                    !Number.isFinite(row.plannedHours) ||
                    row.plannedHours <= 0
                ) {
                    errors.push(
                        `Строка ${row.rowNumber}: не заполнены code/title/teacherId или некорректные plannedHours`,
                    );
                    continue;
                }

                let parsedEquipment: CourseEquipmentItemDto[] = [];
                if (row.equipmentJson) {
                    try {
                        const raw = JSON.parse(row.equipmentJson);
                        if (Array.isArray(raw)) {
                            parsedEquipment = raw
                                .map((it: any) => ({
                                    name: String(it?.name ?? '').trim(),
                                    quantity: Number.parseInt(String(it?.quantity ?? '0'), 10),
                                }))
                                .filter(it => it.name && Number.isFinite(it.quantity) && it.quantity > 0);
                        } else {
                            errors.push(`Строка ${row.rowNumber}: equipmentJson должен быть массивом`);
                        }
                    } catch (err) {
                        console.error(err);
                        errors.push(`Строка ${row.rowNumber}: ошибка парсинга equipmentJson`);
                    }
                }

                try {
                    const body: CreateCourseRequest = {
                        code: row.code,
                        title: row.title,
                        teacherId: row.teacherId,
                        plannedHours: row.plannedHours,
                        equipmentRequirements: parsedEquipment,
                        groupCodes: [],
                    };
                    const created = await coursesApi.create(body);
                    createdCourses.push(created);
                    createdCount++;
                } catch (err: any) {
                    console.error(err);
                    errors.push(
                        `Строка ${row.rowNumber}: ${err?.body?.message ?? 'ошибка создания курса'}`,
                    );
                }
            }

            if (createdCourses.length > 0) {
                setItems(prev => [...prev, ...createdCourses]);
            }

            const groupTasks: Array<() => Promise<void>> = [];
            for (const row of rows) {
                const created = createdCourses.find(c => c.code === row.code);
                if (!created) continue;
                for (const gc of row.groupCodes ?? []) {
                    groupTasks.push(async () => {
                        await coursesApi.addGroup(created.id, gc);
                    });
                }
            }

            if (groupTasks.length > 0) {
                const {errors: groupErrors} = await runWithLimit(groupTasks, 8);
                if (groupErrors.length) {
                    errors.push(`Ошибок привязки групп: ${groupErrors.length}`);
                }
            }

            const fresh = await coursesApi.getAll();
            setItems(fresh);

            const summary = [
                `Успешно создано курсов: ${createdCount}`,
                groupTasks.length ? `Привязок групп отправлено: ${groupTasks.length}` : '',
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
            'courses.csv',
            [
                'code',
                'title',
                'teacherId',
                'plannedHours',
                'requiredRoomCapacity',
                'status',
                'createdAt',
                'updatedAt',
                'equipmentJson',
                'groupCodes',
            ],
            items.map(c => [
                c.code,
                c.title,
                c.teacherId,
                c.plannedHours,
                c.requiredRoomCapacity,
                c.status,
                c.createdAt,
                c.updatedAt,
                JSON.stringify(c.equipmentRequirements ?? []),
                c.groupCodes.join(';'),
            ]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать курс"
            onCreate={openCreateForm}
        />
    );

    return (
        <Page title="Курсы" actions={actions}>
            <CsvMessages error={csvError} result={csvResult} />

            {isAdmin && groupsCourseId && currentCourse && (
                <form className={styles.groupsForm} onSubmit={handleAddGroup}>
                    <div className={styles.groupsHeader}>
                        <strong>Группы курса {currentCourse.code}</strong>
                        <Button type="button" variant="ghost" onClick={closeGroupsForm}>
                            Закрыть
                        </Button>
                    </div>

                    <ul className={styles.groupsList}>
                        {currentCourseGroups.length === 0 && (
                            <li className={styles.empty}>
                                Для курса пока не привязано ни одной группы.
                            </li>
                        )}
                        {currentCourseGroups.map(g => (
                            <li key={g.id} className={styles.groupsRow}>
                                <span>
                                    {g.name}{' '}
                                    <span className={styles.groupCode}>({g.code})</span>
                                </span>
                                <Button
                                    type="button"
                                    variant="ghost"
                                    onClick={() => handleRemoveGroup(g.code)}
                                    disabled={groupsProcessing}
                                >
                                    Удалить
                                </Button>
                            </li>
                        ))}
                    </ul>

                    <div className={styles.selectorRow}>
                        <select
                            className={styles.groupsSelect}
                            value={groupToAddCode}
                            onChange={e => setGroupToAddCode(e.target.value)}
                        >
                            <option value="">Выберите группу…</option>
                            {availableGroups.map(g => (
                                <option key={g.id} value={g.code}>
                                    {g.name} ({g.code})
                                </option>
                            ))}
                        </select>
                        <Button type="submit" disabled={!groupToAddCode || groupsProcessing}>
                            Добавить группу
                        </Button>
                    </div>

                    {groupsError && <p className={styles.groupsError}>{groupsError}</p>}

                    <p className={styles.groupsHint}>
                        Привязка групп влияет на генерацию расписания. В CSV используется колонка{' '}
                        <code>groupCodes</code> — список кодов групп, разделённых точкой с запятой.
                    </p>
                </form>
            )}

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Код курса">
                        <Input value={code} onChange={e => setCode(e.target.value)} />
                    </FormField>
                    <FormField label="Название">
                        <Input value={title} onChange={e => setTitle(e.target.value)} />
                    </FormField>
                    <FormField label="ID Преподавателя">
                        <Input value={teacherId} onChange={e => setTeacherId(e.target.value)} />
                    </FormField>
                    <FormField label="Плановые часы">
                        <Input
                            type="number"
                            min={1}
                            value={plannedHours}
                            onChange={e => setPlannedHours(e.target.value)}
                        />
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

                    <div className={styles.itemsBlock}>
                        <div className={styles.itemsHeader}>
                            <strong>Требуемое оборудование</strong>
                            {equipment.length > 0 && (
                                <Button type="button" variant="ghost" onClick={() => setEquipment([])}>
                                    Очистить
                                </Button>
                            )}
                        </div>

                        {equipment.map((it, idx) => (
                            <div key={idx} className={styles.itemsRow}>
                                <Input
                                    placeholder="Название (проектор, ПК…)"
                                    value={it.name}
                                    onChange={e => handleEquipmentChange(idx, 'name', e.target.value)}
                                />
                                <Input
                                    type="number"
                                    min={1}
                                    placeholder="Кол-во"
                                    value={it.quantity ? String(it.quantity) : ''}
                                    onChange={e => handleEquipmentChange(idx, 'quantity', e.target.value)}
                                />
                                <Button type="button" variant="ghost" onClick={() => handleRemoveEquipment(idx)}>
                                    Удалить
                                </Button>
                            </div>
                        ))}

                        <div className={styles.itemsFooter}>
                            <Button type="button" variant="secondary" onClick={handleAddEquipment}>
                                Добавить требование
                            </Button>
                        </div>

                        <p className={styles.itemsHint}>
                            Требуемое оборудование используется при подборе аудиторий. В CSV оно хранится в колонке{' '}
                            <code>equipmentJson</code> как JSON-массив <code>{'{name, quantity}'}</code>.
                        </p>
                    </div>
                </form>
            )}

            {loading && <p>Загрузка…</p>}

            {!loading && (
                <DataTable>
                    <thead>
                    <tr>
                        <th align="left">Код</th>
                        <th align="left">Название</th>
                        <th align="left">Преподаватель</th>
                        <th align="left">Часы</th>
                        <th align="left">Требуемая вместимость</th>
                        <th align="left">Оборудование</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(c => (
                        <tr key={c.id}>
                            <td>{c.code}</td>
                            <td>{c.title}</td>
                            <td>{c.teacherId}</td>
                            <td>{c.plannedHours}</td>
                            <td>{c.requiredRoomCapacity}</td>
                            <td className={styles.itemsSummary}>{renderEquipment(c.equipmentRequirements ?? [])}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button variant="ghost" type="button" onClick={() => openEditForm(c)}>
                                        Редактировать
                                    </Button>
                                    <Button variant="ghost" type="button" onClick={() => openGroupsForm(c)}>
                                        Группы
                                    </Button>
                                    {c.status === 'ACTIVE' ? (
                                        <Button variant="ghost" type="button" onClick={() => handleArchive(c.id)}>
                                            Архивировать
                                        </Button>
                                    ) : (
                                        <Button variant="ghost" type="button" onClick={() => handleActivate(c.id)}>
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 7 : 6}>Нет курсов</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
