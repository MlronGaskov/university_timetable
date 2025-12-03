import React, {useEffect, useState} from 'react';
import '../AdminCommon.css';
import './PoliciesPage.css';
import {policiesApi} from '@/api/policies';
import type {
    PolicyResponse,
    CreatePolicyRequest,
    UpdatePolicyRequest,
} from '@/types/policy';
import type {UUID} from '@/types/common';
import {FormErrorAlert} from '@/components/common/FormErrorAlert/FormErrorAlert';
import type {ApiErrorResponse} from '@/types/api-error';
import {parseApiErrorBody} from '@/types/api-error';

const emptyJson = '{\n  \n}';

type Mode = 'create' | 'edit';

export const PoliciesPage: React.FC = () => {
    const [policies, setPolicies] = useState<PolicyResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [pageError, setPageError] = useState<string | null>(null);
    const [apiError, setApiError] = useState<ApiErrorResponse | null>(null);

    const [modalOpen, setModalOpen] = useState(false);
    const [mode, setMode] = useState<Mode>('create');
    const [editingPolicy, setEditingPolicy] = useState<PolicyResponse | null>(null);

    const [name, setName] = useState('');
    const [gridJson, setGridJson] = useState(emptyJson);
    const [breaksJson, setBreaksJson] = useState(emptyJson);
    const [limitsJson, setLimitsJson] = useState(emptyJson);
    const [travelMatrixJson, setTravelMatrixJson] = useState(emptyJson);
    const [weightsJson, setWeightsJson] = useState(emptyJson);

    const load = async () => {
        try {
            setLoading(true);
            setPageError(null);
            const data = await policiesApi.getAll();
            setPolicies(data);
        } catch {
            setPageError('Не удалось загрузить политики');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        void load();
    }, []);

    const openCreateModal = () => {
        setMode('create');
        setEditingPolicy(null);
        setName('');
        setGridJson(emptyJson);
        setBreaksJson(emptyJson);
        setLimitsJson(emptyJson);
        setTravelMatrixJson(emptyJson);
        setWeightsJson(emptyJson);
        setApiError(null);
        setModalOpen(true);
    };

    const openEditModal = (p: PolicyResponse) => {
        setMode('edit');
        setEditingPolicy(p);
        setName(p.name);
        setGridJson(p.gridJson);
        setBreaksJson(p.breaksJson);
        setLimitsJson(p.limitsJson);
        setTravelMatrixJson(p.travelMatrixJson);
        setWeightsJson(p.weightsJson);
        setApiError(null);
        setModalOpen(true);
    };

    const closeModal = () => {
        setModalOpen(false);
        setEditingPolicy(null);
        setApiError(null);
    };

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setApiError(null);

        try {
            if (mode === 'create') {
                const payload: CreatePolicyRequest = {
                    name,
                    gridJson,
                    breaksJson,
                    limitsJson,
                    travelMatrixJson,
                    weightsJson,
                };
                await policiesApi.create(payload);
            } else if (mode === 'edit' && editingPolicy) {
                const payload: UpdatePolicyRequest = {
                    name,
                    gridJson,
                    breaksJson,
                    limitsJson,
                    travelMatrixJson,
                    weightsJson,
                };
                await policiesApi.update(editingPolicy.id as UUID, payload);
            }

            await load();
            closeModal();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const handleDelete = async (p: PolicyResponse) => {
        if (!window.confirm(`Удалить политику "${p.name}"?`)) return;

        try {
            await policiesApi.delete(p.id as UUID);
            await load();
        } catch {
            setPageError('Не удалось удалить политику');
        }
    };

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Политики генерации расписания</h1>
                        <p className="Page-headerSubtitle">
                            Управление конфигурациями (сетка, перерывы, лимиты, матрица перемещений, веса).
                        </p>
                    </div>
                    <button className="PrimaryButton" onClick={openCreateModal}>
                        + Новая политика
                    </button>
                </div>

                {pageError && <div className="AdminError">{pageError}</div>}
                {loading && <div>Загрузка...</div>}

                {!loading && !pageError && (
                    <div className="AdminTableWrapper">
                        <table className="AdminTable">
                            <thead>
                            <tr>
                                <th>ID</th>
                                <th>Название</th>
                                <th>Создана</th>
                                <th>Обновлена</th>
                                <th/>
                            </tr>
                            </thead>
                            <tbody>
                            {policies.map(p => (
                                <tr key={p.id}>
                                    <td><code>{p.id}</code></td>
                                    <td>{p.name}</td>
                                    <td>{new Date(p.createdAt).toLocaleString()}</td>
                                    <td>{new Date(p.updatedAt).toLocaleString()}</td>
                                    <td className="AdminActionsCell">
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => openEditModal(p)}
                                        >
                                            Открыть
                                        </button>
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => handleDelete(p)}
                                        >
                                            Удалить
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {policies.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="AdminEmptyCell">
                                        Политики не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {modalOpen && (
                    <div className="Page-modal">
                        <div className="Page-modalCard Policies-modalCard">
                            <form onSubmit={handleSubmit} className="Policies-form">
                                <h2>{mode === 'create' ? 'Новая политика' : 'Редактирование политики'}</h2>

                                <div className="Policies-field">
                                    <span>Название</span>
                                    <input
                                        type="text"
                                        value={name}
                                        onChange={e => setName(e.target.value)}
                                        required
                                    />
                                </div>

                                <div className="Policies-json-grid">
                                    <div className="Policies-field">
                                        <span>gridJson</span>
                                        <textarea
                                            value={gridJson}
                                            onChange={e => setGridJson(e.target.value)}
                                            rows={6}
                                        />
                                    </div>
                                    <div className="Policies-field">
                                        <span>breaksJson</span>
                                        <textarea
                                            value={breaksJson}
                                            onChange={e => setBreaksJson(e.target.value)}
                                            rows={6}
                                        />
                                    </div>
                                    <div className="Policies-field">
                                        <span>limitsJson</span>
                                        <textarea
                                            value={limitsJson}
                                            onChange={e => setLimitsJson(e.target.value)}
                                            rows={6}
                                        />
                                    </div>
                                    <div className="Policies-field">
                                        <span>travelMatrixJson</span>
                                        <textarea
                                            value={travelMatrixJson}
                                            onChange={e => setTravelMatrixJson(e.target.value)}
                                            rows={6}
                                        />
                                    </div>
                                    <div className="Policies-field">
                                        <span>weightsJson</span>
                                        <textarea
                                            value={weightsJson}
                                            onChange={e => setWeightsJson(e.target.value)}
                                            rows={6}
                                        />
                                    </div>
                                </div>

                                <FormErrorAlert apiError={apiError}/>

                                <div className="UserForm-actions">
                                    <button
                                        type="button"
                                        className="UserForm-cancelButton"
                                        onClick={closeModal}
                                    >
                                        Отмена
                                    </button>
                                    <button type="submit" className="PrimaryButton">
                                        Сохранить
                                    </button>
                                </div>
                            </form>
                        </div>
                    </div>
                )}
            </div>
        </div>
    );
};
