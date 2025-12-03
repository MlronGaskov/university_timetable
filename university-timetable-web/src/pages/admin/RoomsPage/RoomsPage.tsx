import React, {useEffect, useState} from 'react';
import '../AdminCommon.css';
import './RoomsPage.css';
import {roomsApi} from '@/api/rooms';
import type {
    RoomResponse,
    CreateRoomRequest,
    UpdateRoomRequest,
    RoomItemDto,
} from '@/types/room';
import type {UUID} from '@/types/common';
import {FormErrorAlert} from '@/components/common/FormErrorAlert/FormErrorAlert';
import type {ApiErrorResponse} from '@/types/api-error';
import {parseApiErrorBody} from '@/types/api-error';

export const RoomsPage: React.FC = () => {
    const [rooms, setRooms] = useState<RoomResponse[]>([]);
    const [loading, setLoading] = useState(true);
    const [pageError, setPageError] = useState<string | null>(null);
    const [apiError, setApiError] = useState<ApiErrorResponse | null>(null);

    const [building, setBuilding] = useState('');
    const [number, setNumber] = useState('');
    const [capacity, setCapacity] = useState(0);

    const [editModalOpen, setEditModalOpen] = useState(false);
    const [editingRoom, setEditingRoom] = useState<RoomResponse | null>(null);
    const [editBuilding, setEditBuilding] = useState('');
    const [editNumber, setEditNumber] = useState('');
    const [editCapacity, setEditCapacity] = useState(0);
    const [editItems, setEditItems] = useState<RoomItemDto[]>([]);

    const load = async () => {
        try {
            setLoading(true);
            setPageError(null);
            const data = await roomsApi.getAll();
            setRooms(data);
        } catch {
            setPageError('Не удалось загрузить аудитории');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        void load();
    }, []);

    const handleCreate = async (e: React.FormEvent) => {
        e.preventDefault();
        setApiError(null);
        try {
            const payload: CreateRoomRequest = {
                building,
                number,
                capacity,
                items: [],
            };
            await roomsApi.create(payload);
            setBuilding('');
            setNumber('');
            setCapacity(0);
            await load();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const openEditModal = (r: RoomResponse) => {
        setEditingRoom(r);
        setEditBuilding(r.building);
        setEditNumber(r.number);
        setEditCapacity(r.capacity);
        setEditItems(r.items);
        setApiError(null);
        setEditModalOpen(true);
    };

    const closeEditModal = () => {
        setEditModalOpen(false);
        setEditingRoom(null);
        setApiError(null);
    };

    const handleToggleStatus = async (r: RoomResponse) => {
        try {
            setPageError(null);
            if (r.status === 'ACTIVE') {
                await roomsApi.archive(r.id as UUID);
            } else {
                await roomsApi.activate(r.id as UUID);
            }
            await load();
        } catch {
            setPageError('Не удалось изменить статус аудитории');
        }
    };

    const handleItemChange = (index: number, patch: Partial<RoomItemDto>) => {
        setEditItems(prev =>
            prev.map((it, i) => (i === index ? {...it, ...patch} : it))
        );
    };

    const handleAddItem = () => {
        setEditItems(prev => [...prev, {name: '', quantity: 1}]);
    };

    const handleRemoveItem = (index: number) => {
        setEditItems(prev => prev.filter((_, i) => i !== index));
    };

    const handleEditSave = async (e: React.FormEvent) => {
        e.preventDefault();
        if (!editingRoom) return;

        setApiError(null);
        try {
            const cleanItems = editItems
                .filter(it => it.name.trim().length > 0)
                .map<RoomItemDto>(it => ({
                    name: it.name,
                    quantity: it.quantity ?? 0,
                }));

            const payload: UpdateRoomRequest = {
                building: editBuilding,
                number: editNumber,
                capacity: editCapacity,
                items: cleanItems,
            };

            await roomsApi.update(editingRoom.id as UUID, payload);
            await load();
            closeEditModal();
        } catch (err: any) {
            const parsed = parseApiErrorBody(err.body);
            if (parsed) setApiError(parsed);
        }
    };

    const formatItems = (r: RoomResponse) =>
        r.items.length === 0
            ? '—'
            : r.items.map(it => `${it.name} (${it.quantity})`).join(', ');

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Аудитории</h1>
                        <p className="Page-headerSubtitle">
                            Каталог аудиторий. Администратор может задавать оборудование, архивировать и
                            изменять параметры.
                        </p>
                    </div>
                </div>

                <form className="SimpleForm" onSubmit={handleCreate}>
                    <input
                        type="text"
                        placeholder="Корпус"
                        value={building}
                        onChange={e => setBuilding(e.target.value)}
                    />
                    <input
                        type="text"
                        placeholder="Номер"
                        value={number}
                        onChange={e => setNumber(e.target.value)}
                    />
                    <input
                        type="number"
                        placeholder="Вместимость"
                        value={capacity}
                        onChange={e => setCapacity(Number(e.target.value))}
                    />
                    <button className="PrimaryButton" type="submit">
                        Добавить
                    </button>
                </form>

                <FormErrorAlert apiError={apiError}/>

                {pageError && <div className="AdminError">{pageError}</div>}
                {loading && <div>Загрузка...</div>}

                {!loading && !pageError && (
                    <div className="AdminTableWrapper">
                        <table className="AdminTable">
                            <thead>
                            <tr>
                                <th>ID</th>
                                <th>Корпус</th>
                                <th>Номер</th>
                                <th>Вместимость</th>
                                <th>Статус</th>
                                <th>Оборудование</th>
                                <th>Создана</th>
                                <th>Обновлена</th>
                                <th/>
                            </tr>
                            </thead>
                            <tbody>
                            {rooms.map(r => (
                                <tr key={r.id}>
                                    <td><code>{r.id}</code></td>
                                    <td>{r.building}</td>
                                    <td>{r.number}</td>
                                    <td>{r.capacity}</td>
                                    <td>{r.status}</td>
                                    <td>{formatItems(r)}</td>
                                    <td>{new Date(r.createdAt).toLocaleString()}</td>
                                    <td>{new Date(r.updatedAt).toLocaleString()}</td>
                                    <td className="AdminActionsCell">
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => openEditModal(r)}
                                        >
                                            Редактировать
                                        </button>
                                        <button
                                            type="button"
                                            className="AdminSmallButton"
                                            onClick={() => handleToggleStatus(r)}
                                        >
                                            {r.status === 'ACTIVE' ? 'Архивировать' : 'Активировать'}
                                        </button>
                                    </td>
                                </tr>
                            ))}
                            {rooms.length === 0 && (
                                <tr>
                                    <td colSpan={9} className="AdminEmptyCell">
                                        Аудитории не найдены
                                    </td>
                                </tr>
                            )}
                            </tbody>
                        </table>
                    </div>
                )}

                {editModalOpen && editingRoom && (
                    <div className="ModalBackdrop">
                        <div className="ModalCard">
                            <form onSubmit={handleEditSave}>
                                <h2>
                                    Аудитория {editingRoom.building} {editingRoom.number}
                                </h2>

                                <div className="AdminFormField">
                                    <span>Корпус</span>
                                    <input
                                        type="text"
                                        value={editBuilding}
                                        onChange={e => setEditBuilding(e.target.value)}
                                    />
                                </div>

                                <div className="AdminFormField">
                                    <span>Номер</span>
                                    <input
                                        type="text"
                                        value={editNumber}
                                        onChange={e => setEditNumber(e.target.value)}
                                    />
                                </div>

                                <div className="AdminFormField">
                                    <span>Вместимость</span>
                                    <input
                                        type="text"
                                        value={editCapacity}
                                        onChange={e => setEditCapacity(Number(e.target.value))}
                                    />
                                </div>

                                <div className="AdminFormField">
                                    <span>Оборудование</span>
                                    <div className="AdminForm">
                                        {editItems.map((it, idx) => (
                                            <div className="AdminFormRow" key={idx}>
                                                <input
                                                    type="text"
                                                    placeholder="Название"
                                                    value={it.name}
                                                    onChange={e =>
                                                        handleItemChange(idx, {name: e.target.value})
                                                    }
                                                />
                                                <input
                                                    type="number"
                                                    placeholder="Кол-во"
                                                    value={it.quantity}
                                                    onChange={e =>
                                                        handleItemChange(idx, {
                                                            quantity: Number(e.target.value),
                                                        })
                                                    }
                                                />
                                                <button
                                                    type="button"
                                                    className="SecondaryButton"
                                                    onClick={() => handleRemoveItem(idx)}
                                                >
                                                    Удалить
                                                </button>
                                            </div>
                                        ))}
                                    </div>
                                    <button
                                        type="button"
                                        className="SecondaryButton"
                                        onClick={handleAddItem}
                                        style={{marginTop: 4}}
                                    >
                                        + Добавить оборудование
                                    </button>
                                </div>

                                <FormErrorAlert apiError={apiError}/>

                                <div className="FormActions">
                                    <button
                                        type="button"
                                        className="SecondaryButton"
                                        onClick={closeEditModal}
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
