import React, {useEffect, useMemo, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {roomsApi} from '@/api/rooms';
import type {
    CreateRoomRequest,
    RoomItemDto,
    RoomResponse,
    UpdateRoomRequest,
} from '@/types/rooms';
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
import styles from './RoomsListPage.module.css';

interface CsvRow {
    roomCode: string;
    building: string;
    number: string;
    capacity: number;
    itemsJson: string | null;
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

    const idxRoomCode = idx('roomCode');
    const idxBuilding = idx('building');
    const idxNumber = idx('number');
    const idxCapacity = idx('capacity');
    const idxItemsJson = idx('itemsJson');

    if (idxRoomCode < 0 || idxBuilding < 0 || idxNumber < 0 || idxCapacity < 0) {
        throw new Error(
            'Ожидаются колонки как минимум: roomCode,building,number,capacity[,itemsJson]',
        );
    }

    const rows: CsvRow[] = [];
    for (let i = 1; i < lines.length; i++) {
        const rawLine = lines[i].trim();
        if (!rawLine) continue;

        const parts = splitCsvLine(rawLine);
        const safe = (idxN: number) => (parts[idxN] ?? '').trim();

        const cap = Number.parseInt(safe(idxCapacity), 10);

        rows.push({
            roomCode: safe(idxRoomCode),
            building: safe(idxBuilding),
            number: safe(idxNumber),
            capacity: cap,
            itemsJson: idxItemsJson >= 0 ? safe(idxItemsJson) || null : null,
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

export const RoomsListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<RoomResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);
    const [roomCode, setRoomCode] = useState('');
    const [building, setBuilding] = useState('');
    const [number, setNumber] = useState('');
    const [capacity, setCapacity] = useState('');
    const [roomItems, setRoomItems] = useState<RoomItemDto[]>([]);
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await roomsApi.getAll();
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
        setRoomCode('');
        setBuilding('');
        setNumber('');
        setCapacity('');
        setRoomItems([]);
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (r: RoomResponse) => {
        setFormMode('edit');
        setEditingId(r.id);
        setRoomCode(r.roomCode);
        setBuilding(r.building);
        setNumber(r.number);
        setCapacity(String(r.capacity));
        setRoomItems(r.items ?? []);
        setFormError(null);
    };

    const handleItemChange = (
        index: number,
        field: 'name' | 'quantity',
        value: string,
    ) => {
        setRoomItems(prev =>
            prev.map((it, i) => {
                if (i !== index) return it;
                if (field === 'name') {
                    return {...it, name: value};
                }
                const q = Number.parseInt(value, 10);
                return {...it, quantity: Number.isFinite(q) && q > 0 ? q : 0};
            }),
        );
    };

    const handleAddItem = () => {
        setRoomItems(prev => [...prev, {name: '', quantity: 1}]);
    };

    const handleRemoveItem = (index: number) => {
        setRoomItems(prev => prev.filter((_, i) => i !== index));
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!roomCode.trim() || !building.trim() || !number.trim() || !capacity.trim()) {
            setFormError('Заполните код, корпус, номер и вместимость');
            return;
        }

        const parsedCap = Number.parseInt(capacity.trim(), 10);
        if (!Number.isFinite(parsedCap) || parsedCap <= 0) {
            setFormError('Вместимость должна быть положительным числом');
            return;
        }

        const normalizedItems: RoomItemDto[] = (roomItems ?? [])
            .map(it => ({
                name: String(it.name ?? '').trim(),
                quantity: Number.parseInt(String(it.quantity ?? '0'), 10),
            }))
            .filter(it => it.name && Number.isFinite(it.quantity) && it.quantity > 0);

        setSaving(true);
        setFormError(null);

        try {
            if (formMode === 'create') {
                const body: CreateRoomRequest = {
                    roomCode: roomCode.trim(),
                    building: building.trim(),
                    number: number.trim(),
                    capacity: parsedCap,
                    items: normalizedItems.length > 0 ? normalizedItems : undefined,
                };
                const created = await roomsApi.create(body);
                setItems(prev => [...prev, created]);
            } else if (formMode === 'edit' && editingId) {
                const body: UpdateRoomRequest = {
                    roomCode: roomCode.trim(),
                    building: building.trim(),
                    number: number.trim(),
                    capacity: parsedCap,
                    items: normalizedItems,
                };
                const updated = await roomsApi.update(editingId, body);
                setItems(prev => prev.map(r => (r.id === updated.id ? updated : r)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения аудитории');
        } finally {
            setSaving(false);
        }
    };

    const handleArchive = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await roomsApi.archive(id);
            setItems(prev => prev.map(r => (r.id === updated.id ? updated : r)));
        } catch (e) {
            console.error(e);
            alert('Не удалось архивировать аудиторию');
        }
    };

    const handleActivate = async (id: string) => {
        if (!isAdmin) return;
        try {
            const updated = await roomsApi.activate(id);
            setItems(prev => prev.map(r => (r.id === updated.id ? updated : r)));
        } catch (e) {
            console.error(e);
            alert('Не удалось активировать аудиторию');
        }
    };

    const itemsTitle = (its: RoomItemDto[]): string => {
        if (!its || its.length === 0) return '—';
        return its.map(i => `${i.name}×${i.quantity}`).join('\n');
    };

    const renderItems = (its: RoomItemDto[]) => {
        if (!its || its.length === 0) return '—';
        return (
            <div title={itemsTitle(its)}>
                {its.map((i, idx) => (
                    <React.Fragment key={`${i.name}-${idx}`}>
                        {i.name}×{i.quantity}
                        {idx < its.length - 1 ? <br /> : null}
                    </React.Fragment>
                ))}
            </div>
        );
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
            const createTasks: Array<() => Promise<RoomResponse>> = [];

            for (const row of rows) {
                if (
                    !row.roomCode ||
                    !row.building ||
                    !row.number ||
                    !Number.isFinite(row.capacity) ||
                    row.capacity <= 0
                ) {
                    validationErrors.push(
                        `Строка ${row.rowNumber}: не заполнены roomCode/building/number или некорректная capacity`,
                    );
                    continue;
                }

                let parsedItems: RoomItemDto[] | undefined;
                if (row.itemsJson) {
                    try {
                        const raw = JSON.parse(row.itemsJson);
                        if (Array.isArray(raw)) {
                            parsedItems = raw
                                .map((it: any) => ({
                                    name: String(it?.name ?? '').trim(),
                                    quantity: Number.parseInt(String(it?.quantity ?? '0'), 10),
                                }))
                                .filter(it => it.name && Number.isFinite(it.quantity) && it.quantity > 0);
                        } else {
                            validationErrors.push(
                                `Строка ${row.rowNumber}: itemsJson должен быть массивом`,
                            );
                        }
                    } catch (err) {
                        console.error(err);
                        validationErrors.push(
                            `Строка ${row.rowNumber}: ошибка парсинга itemsJson`,
                        );
                    }
                }

                createTasks.push(async () => {
                    const body: CreateRoomRequest = {
                        roomCode: row.roomCode,
                        building: row.building,
                        number: row.number,
                        capacity: row.capacity,
                        items: parsedItems && parsedItems.length > 0 ? parsedItems : undefined,
                    };
                    return await roomsApi.create(body);
                });
            }

            const {results: created, errors: createErrors} = await runWithLimit(createTasks, 8);

            if (created.length > 0) {
                setItems(prev => [...prev, ...created]);
            }

            const errors: string[] = [...validationErrors];
            for (const e of createErrors) {
                errors.push(e?.body?.message ?? 'ошибка создания аудитории');
            }

            const summary = [
                `Успешно создано аудиторий: ${created.length}`,
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
            'rooms.csv',
            [
                'roomCode',
                'building',
                'number',
                'capacity',
                'status',
                'createdAt',
                'updatedAt',
                'itemsJson',
            ],
            items.map(r => [
                r.roomCode,
                r.building,
                r.number,
                r.capacity,
                r.status,
                r.createdAt,
                r.updatedAt,
                JSON.stringify(r.items ?? []),
            ]),
        );
    };

    const actions = useMemo(
        () => (
            <CsvToolbar
                onExport={exportCsv}
                onImportFile={importFromFile}
                importDisabled={csvProcessing}
                showCreate={isAdmin}
                createLabel="Создать аудиторию"
                onCreate={openCreateForm}
            />
        ),
        [csvProcessing, isAdmin, items],
    );

    return (
        <Page title="Аудитории" actions={actions}>
            <CsvMessages error={csvError} result={csvResult} />

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Код аудитории">
                        <Input value={roomCode} onChange={e => setRoomCode(e.target.value)} />
                    </FormField>
                    <FormField label="Корпус">
                        <Input value={building} onChange={e => setBuilding(e.target.value)} />
                    </FormField>
                    <FormField label="Номер">
                        <Input value={number} onChange={e => setNumber(e.target.value)} />
                    </FormField>
                    <FormField label="Вместимость">
                        <Input
                            type="number"
                            min={1}
                            value={capacity}
                            onChange={e => setCapacity(e.target.value)}
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
                            <strong>Оборудование аудитории</strong>
                            {roomItems.length > 0 && (
                                <Button type="button" variant="ghost" onClick={() => setRoomItems([])}>
                                    Очистить
                                </Button>
                            )}
                        </div>

                        {roomItems.map((it, idx) => (
                            <div key={idx} className={styles.itemsRow}>
                                <Input
                                    placeholder="Название (проектор, доска…)"
                                    value={it.name}
                                    onChange={e => handleItemChange(idx, 'name', e.target.value)}
                                />
                                <Input
                                    type="number"
                                    min={1}
                                    placeholder="Кол-во"
                                    value={it.quantity ? String(it.quantity) : ''}
                                    onChange={e => handleItemChange(idx, 'quantity', e.target.value)}
                                />
                                <Button type="button" variant="ghost" onClick={() => handleRemoveItem(idx)}>
                                    Удалить
                                </Button>
                            </div>
                        ))}

                        <div className={styles.itemsFooter}>
                            <Button type="button" variant="secondary" onClick={handleAddItem}>
                                Добавить оборудование
                            </Button>
                        </div>

                        <p className={styles.itemsHint}>
                            Оборудование используется при подборе подходящих аудиторий для курсов. В CSV колонка{' '}
                            <code>itemsJson</code> содержит JSON-массив объектов <code>{'{name, quantity}'}</code>.
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
                        <th align="left">Аудитория</th>
                        <th align="left">Вместимость</th>
                        <th align="left">Оборудование</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(r => (
                        <tr key={r.id}>
                            <td>{r.roomCode}</td>
                            <td>{r.building} {r.number}</td>
                            <td>{r.capacity}</td>
                            <td className={styles.itemsSummary}>{renderItems(r.items ?? [])}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button variant="ghost" type="button" onClick={() => openEditForm(r)}>
                                        Редактировать
                                    </Button>
                                    {r.status === 'ACTIVE' ? (
                                        <Button variant="ghost" type="button" onClick={() => handleArchive(r.id)}>
                                            Архивировать
                                        </Button>
                                    ) : (
                                        <Button variant="ghost" type="button" onClick={() => handleActivate(r.id)}>
                                            Активировать
                                        </Button>
                                    )}
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 5 : 4}>Нет аудиторий</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
