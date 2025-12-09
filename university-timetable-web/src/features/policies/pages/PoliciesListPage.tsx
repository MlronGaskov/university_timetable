import React, {useEffect, useState} from 'react';
import {Page} from '@/components/layout/Page';
import {policiesApi} from '@/api/policies';
import type {CreatePolicyRequest, PolicyResponse, UpdatePolicyRequest,} from '@/types/policies';
import {useRoleGuard} from '@/hooks/useRoleGuard';
import {Input} from '@/components/ui/Input';
import {FormField} from '@/components/ui/FormField';
import {Button} from '@/components/ui/Button';
import {DataTable} from '@/components/ui/DataTable';
import {ActionsCell} from '@/components/ui/ActionsCell';
import {CsvMessages} from '@/components/ui/CsvMessages';
import {CsvToolbar} from '@/components/ui/CsvToolbar';
import {downloadCsv} from '@/utils/csv';
import {formatInstant} from '@/utils/formatters';
import crudStyles from '@/components/ui/CrudFormLayout.module.css';
import styles from './PoliciesListPage.module.css';

interface CsvRow {
    name: string;
    gridJson: string;
    breaksJson: string;
    limitsJson: string;
    travelMatrixJson: string;
    weightsJson: string;
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

export const PoliciesListPage: React.FC = () => {
    const {isAdmin} = useRoleGuard();

    const [items, setItems] = useState<PolicyResponse[]>([]);
    const [loading, setLoading] = useState(false);

    const [formMode, setFormMode] = useState<'create' | 'edit' | null>(null);
    const [editingId, setEditingId] = useState<string | null>(null);

    const [name, setName] = useState('');
    const [gridJson, setGridJson] = useState('');
    const [breaksJson, setBreaksJson] = useState('');
    const [limitsJson, setLimitsJson] = useState('');
    const [travelMatrixJson, setTravelMatrixJson] = useState('');
    const [weightsJson, setWeightsJson] = useState('');
    const [formError, setFormError] = useState<string | null>(null);
    const [saving, setSaving] = useState(false);

    const [csvError, setCsvError] = useState<string | null>(null);
    const [csvResult, setCsvResult] = useState<string | null>(null);
    const [csvProcessing, setCsvProcessing] = useState(false);

    useEffect(() => {
        (async () => {
            setLoading(true);
            try {
                const data = await policiesApi.getAll();
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
        setName('');
        setGridJson('');
        setBreaksJson('');
        setLimitsJson('');
        setTravelMatrixJson('');
        setWeightsJson('');
        setFormError(null);
    };

    const openCreateForm = () => {
        resetForm();
        setFormMode('create');
    };

    const openEditForm = (p: PolicyResponse) => {
        setFormMode('edit');
        setEditingId(p.id);
        setName(p.name);
        setGridJson(p.gridJson);
        setBreaksJson(p.breaksJson);
        setLimitsJson(p.limitsJson);
        setTravelMatrixJson(p.travelMatrixJson);
        setWeightsJson(p.weightsJson);
        setFormError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!isAdmin) return;

        if (!name.trim()) {
            setFormError('Заполните имя политики');
            return;
        }
        if (!gridJson.trim() || !breaksJson.trim() || !limitsJson.trim()
            || !travelMatrixJson.trim() || !weightsJson.trim()) {
            setFormError('Все JSON-поля должны быть заполнены');
            return;
        }

        const body: CreatePolicyRequest | UpdatePolicyRequest = {
            name: name.trim(),
            gridJson: gridJson.trim(),
            breaksJson: breaksJson.trim(),
            limitsJson: limitsJson.trim(),
            travelMatrixJson: travelMatrixJson.trim(),
            weightsJson: weightsJson.trim(),
        };

        setSaving(true);
        setFormError(null);

        try {
            if (formMode === 'create') {
                const created = await policiesApi.create(body as CreatePolicyRequest);
                setItems(prev => [...prev, created]);
            } else if (formMode === 'edit' && editingId) {
                const updated = await policiesApi.update(editingId, body as UpdatePolicyRequest);
                setItems(prev => prev.map(p => (p.id === updated.id ? updated : p)));
            }
            resetForm();
        } catch (err) {
            console.error(err);
            setFormError('Ошибка сохранения политики (возможно, имя уже занято)');
        } finally {
            setSaving(false);
        }
    };

    const handleDelete = async (id: string) => {
        if (!isAdmin) return;
        if (!confirm('Удалить эту политику?')) return;
        try {
            await policiesApi.delete(id);
            setItems(prev => prev.filter(p => p.id !== id));
        } catch (e) {
            console.error(e);
            alert('Не удалось удалить политику');
        }
    };

    const parseCsv = (text: string): CsvRow[] => {
        const lines = text.trim().split(/\r?\n/);
        if (lines.length < 2) return [];

        const header = splitCsvLine(lines[0]).map(h => h.trim());
        const idx = (name: string) => header.indexOf(name);

        const idxName = idx('name');
        const idxGrid = idx('gridJson');
        const idxBreaks = idx('breaksJson');
        const idxLimits = idx('limitsJson');
        const idxTravel = idx('travelMatrixJson');
        const idxWeights = idx('weightsJson');

        if (
            idxName < 0 ||
            idxGrid < 0 ||
            idxBreaks < 0 ||
            idxLimits < 0 ||
            idxTravel < 0 ||
            idxWeights < 0
        ) {
            throw new Error(
                'Ожидаются колонки: name,gridJson,breaksJson,limitsJson,travelMatrixJson,weightsJson',
            );
        }

        const rows: CsvRow[] = [];
        for (let i = 1; i < lines.length; i++) {
            const line = lines[i].trim();
            if (!line) continue;
            const parts = splitCsvLine(line);
            const safe = (idx: number) => (parts[idx] ?? '').trim();

            rows.push({
                name: safe(idxName),
                gridJson: safe(idxGrid),
                breaksJson: safe(idxBreaks),
                limitsJson: safe(idxLimits),
                travelMatrixJson: safe(idxTravel),
                weightsJson: safe(idxWeights),
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
                if (!row.name || !row.gridJson || !row.breaksJson || !row.limitsJson ||
                    !row.travelMatrixJson || !row.weightsJson) {
                    errors.push(`Строка ${row.rowNumber}: не все поля заполнены`);
                    continue;
                }

                try {
                    const created = await policiesApi.create({
                        name: row.name,
                        gridJson: row.gridJson,
                        breaksJson: row.breaksJson,
                        limitsJson: row.limitsJson,
                        travelMatrixJson: row.travelMatrixJson,
                        weightsJson: row.weightsJson,
                    });
                    success++;
                    setItems(prev => [...prev, created]);
                } catch (err: any) {
                    console.error(err);
                    errors.push(
                        `Строка ${row.rowNumber}: ${
                            err?.body?.message ?? 'ошибка создания политики'
                        }`,
                    );
                }
            }

            const summary = [
                `Успешно создано политик: ${success}`,
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
            'policies.csv',
            [
                'name',
                'gridJson',
                'breaksJson',
                'limitsJson',
                'travelMatrixJson',
                'weightsJson',
                'createdAt',
                'updatedAt',
            ],
            items.map(p => [
                p.name,
                p.gridJson,
                p.breaksJson,
                p.limitsJson,
                p.travelMatrixJson,
                p.weightsJson,
                p.createdAt,
                p.updatedAt,
            ]),
        );
    };

    const actions = (
        <CsvToolbar
            onExport={exportCsv}
            onImportFile={importFromFile}
            importDisabled={csvProcessing}
            showCreate={isAdmin}
            createLabel="Создать политику"
            onCreate={openCreateForm}
        />
    );

    return (
        <Page title="Политики" actions={actions}>
            <CsvMessages error={csvError} result={csvResult}/>

            {formMode && (
                <form className={crudStyles.form} onSubmit={handleSubmit}>
                    <FormField label="Имя политики">
                        <Input value={name} onChange={e => setName(e.target.value)}/>
                    </FormField>

                    <FormField label="Grid JSON">
                        <textarea
                            className={styles.jsonArea}
                            value={gridJson}
                            onChange={e => setGridJson(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Breaks JSON">
                        <textarea
                            className={styles.jsonArea}
                            value={breaksJson}
                            onChange={e => setBreaksJson(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Limits JSON">
                        <textarea
                            className={styles.jsonArea}
                            value={limitsJson}
                            onChange={e => setLimitsJson(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Travel matrix JSON">
                        <textarea
                            className={styles.jsonArea}
                            value={travelMatrixJson}
                            onChange={e => setTravelMatrixJson(e.target.value)}
                        />
                    </FormField>

                    <FormField label="Weights JSON">
                        <textarea
                            className={styles.jsonArea}
                            value={weightsJson}
                            onChange={e => setWeightsJson(e.target.value)}
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

                    {formError && (
                        <div className={crudStyles.formError}>{formError}</div>
                    )}
                </form>
            )}

            {loading && <p>Загрузка…</p>}

            {!loading && (
                <DataTable>
                    <thead>
                    <tr>
                        <th align="left">Имя</th>
                        <th align="left">Создано</th>
                        <th align="left">Обновлено</th>
                        {isAdmin && <th align="left">Действия</th>}
                    </tr>
                    </thead>
                    <tbody>
                    {items.map(p => (
                        <tr key={p.id}>
                            <td>{p.name}</td>
                            <td>{formatInstant(p.createdAt)}</td>
                            <td>{formatInstant(p.updatedAt)}</td>
                            {isAdmin && (
                                <ActionsCell>
                                    <Button
                                        type="button"
                                        variant="ghost"
                                        onClick={() => openEditForm(p)}
                                    >
                                        Редактировать
                                    </Button>
                                    <Button
                                        type="button"
                                        variant="ghost"
                                        onClick={() => handleDelete(p.id)}
                                    >
                                        Удалить
                                    </Button>
                                </ActionsCell>
                            )}
                        </tr>
                    ))}
                    {items.length === 0 && !loading && (
                        <tr>
                            <td colSpan={isAdmin ? 4 : 3}>Нет политик</td>
                        </tr>
                    )}
                    </tbody>
                </DataTable>
            )}
        </Page>
    );
};
