import React, {useEffect, useState} from 'react';
import {useNavigate} from 'react-router-dom';
import Button from '../components/ui/Button';
import Modal from '../components/ui/Modal';
import Input from '../components/ui/Input';
import {RoomDto} from '@/types';
import {RoomsApi, CreateRoomReq, UpdateRoomReq, RoomEquipmentItemReq} from '@api/rooms';
import {useAuth} from '@hooks/useAuth';
import NavBar from "@components/NavBar";

type RoomFormProps = {
    initial?: RoomDto;
    onSubmit: (data: CreateRoomReq | UpdateRoomReq) => Promise<void> | void;
    submitLabel: string;
};

function RoomForm({initial, onSubmit, submitLabel}: RoomFormProps) {
    const isEdit = !!initial;
    const [building, setBuilding] = useState(initial?.building ?? '');
    const [number, setNumber] = useState(initial?.number ?? '');
    const [capacity, setCapacity] = useState(initial?.capacity?.toString() ?? '0');
    const [equipmentName, setEquipmentName] = useState('');
    const [equipmentQty, setEquipmentQty] = useState('1');
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setSaving(true);
        setError(null);
        try {
            const equip: RoomEquipmentItemReq[] = [];
            if (equipmentName.trim()) {
                equip.push({
                    name: equipmentName.trim(),
                    quantity: Number(equipmentQty) || 0,
                });
            }

            const body: any = {
                building: building.trim(),
                number: number.trim(),
                capacity: Number(capacity) || 0,
            };
            if (equip.length) body.equipment = equip;

            await onSubmit(body);
        } catch (err: any) {
            setError(err?.message ?? 'Ошибка');
        } finally {
            setSaving(false);
        }
    };

    return (
        <form onSubmit={handleSubmit}>
            <div className="row">
                <label className="label">Корпус</label>
                <Input value={building} onChange={e => setBuilding(e.currentTarget.value)} required/>
            </div>
            <div className="row">
                <label className="label">Номер аудитории</label>
                <Input value={number} onChange={e => setNumber(e.currentTarget.value)} required/>
            </div>
            <div className="row">
                <label className="label">Вместимость</label>
                <Input
                    type="number"
                    value={capacity}
                    onChange={e => setCapacity(e.currentTarget.value)}
                    required
                    min={0}
                />
            </div>
            <div className="row">
                <label className="label">Оборудование (простая форма)</label>
                <div style={{display: 'grid', gridTemplateColumns: '2fr 1fr', gap: 8}}>
                    <Input
                        placeholder="например, Проектор"
                        value={equipmentName}
                        onChange={e => setEquipmentName(e.currentTarget.value)}
                    />
                    <Input
                        type="number"
                        placeholder="Кол-во"
                        value={equipmentQty}
                        onChange={e => setEquipmentQty(e.currentTarget.value)}
                        min={0}
                    />
                </div>
                <p className="note">Пока только одно оборудование за раз; можно редактировать позже.</p>
            </div>
            {error && <div className="error">{error}</div>}
            <div style={{display: 'flex', justifyContent: 'flex-end', gap: 8, paddingTop: 8}}>
                <Button type="submit" className="primary" disabled={saving}>
                    {submitLabel}
                </Button>
            </div>
        </form>
    );
}

function RoomsTable({rooms, isAdmin, onEdit, onArchive}: {
    rooms: RoomDto[];
    isAdmin: boolean;
    onEdit: (r: RoomDto) => void;
    onArchive: (r: RoomDto) => void;
}) {
    return (
        <div className="card" style={{overflow: 'auto'}}>
            <table className="table">
                <thead>
                <tr>
                    <th>Корпус</th>
                    <th>Номер</th>
                    <th>Вместимость</th>
                    <th>Статус</th>
                    <th>Оборудование</th>
                    <th>Создано</th>
                    <th>Обновлено</th>
                    {isAdmin && <th>Действия</th>}
                </tr>
                </thead>
                <tbody>
                {rooms.map(r => (
                    <tr key={r.id}>
                        <td>{r.building}</td>
                        <td>{r.number}</td>
                        <td>{r.capacity}</td>
                        <td>{r.status}</td>
                        <td>
                            {r.equipment?.length
                                ? r.equipment.map(e => `${e.name} (${e.quantity})`).join(', ')
                                : '—'}
                        </td>
                        <td>{new Date(r.createdAt).toLocaleString()}</td>
                        <td>{new Date(r.updatedAt).toLocaleString()}</td>
                        {isAdmin && (
                            <td style={{textAlign: 'center'}}>
                                <div style={{display: 'flex', gap: 8, justifyContent: 'center', flexWrap: 'wrap'}}>
                                    <Button onClick={() => onEdit(r)}>Редактировать</Button>
                                    <Button
                                        className="warn"
                                        onClick={() => onArchive(r)}
                                        disabled={r.status === 'INACTIVE'}
                                    >
                                        Архивировать
                                    </Button>
                                </div>
                            </td>
                        )}
                    </tr>
                ))}
                </tbody>
            </table>
        </div>
    );
}

export default function RoomsPage() {
    const {isAdmin} = useAuth();
    const [rooms, setRooms] = useState<RoomDto[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);
    const [showCreate, setShowCreate] = useState(false);
    const [editing, setEditing] = useState<RoomDto | null>(null);

    const load = async () => {
        setLoading(true);
        setError(null);
        try {
            setRooms(await RoomsApi.list());
        } catch {
            setError('Ошибка загрузки аудиторий');
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => {
        load();
    }, []);

    return (
        <div className="page">
            <header className="hdr">
                <div className="page-header-title">Аудитории</div>
                <div style={{display: 'flex', gap: 8, alignItems: 'center'}}>
                    <NavBar/>
                    {isAdmin && (
                        <Button onClick={() => setShowCreate(true)} className="primary">
                            Создать
                        </Button>
                    )}
                </div>
            </header>

            <main className="container page-main">
                {loading && <div>Загрузка…</div>}
                {error && <div className="error">{error}</div>}
                {!loading && (
                    <RoomsTable
                        rooms={rooms}
                        isAdmin={isAdmin}
                        onEdit={r => setEditing(r)}
                        onArchive={async r => {
                            if (!window.confirm('Архивировать аудиторию?')) return;
                            await RoomsApi.archive(r.id);
                            load();
                        }}
                    />
                )}
            </main>

            {showCreate && (
                <Modal title="Создать аудиторию" onClose={() => setShowCreate(false)}>
                    <RoomForm
                        submitLabel="Создать"
                        onSubmit={async data => {
                            await RoomsApi.create(data as CreateRoomReq);
                            setShowCreate(false);
                            load();
                        }}
                    />
                </Modal>
            )}

            {editing && (
                <Modal title={`Редактировать аудиторию ${editing.number}`} onClose={() => setEditing(null)}>
                    <RoomForm
                        initial={editing}
                        submitLabel="Сохранить"
                        onSubmit={async data => {
                            await RoomsApi.update(editing.id, data as UpdateRoomReq);
                            setEditing(null);
                            load();
                        }}
                    />
                </Modal>
            )}
        </div>
    );
}
