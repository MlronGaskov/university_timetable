import React, {useEffect, useState} from 'react';
import Button from '../components/ui/Button';
import Modal from '../components/ui/Modal';
import Input from '../components/ui/Input';
import NavBar from '../components/NavBar';
import {GroupDto, SubgroupDto} from '@/types';
import {
    GroupsApi,
    CreateGroupReq,
    UpdateGroupReq,
    CreateSubgroupReq,
    UpdateSubgroupReq
} from '@api/groups';
import {useAuth} from '@hooks/useAuth';

type GroupFormProps = {
    initial?: GroupDto;
    onSubmit: (data: CreateGroupReq | UpdateGroupReq) => Promise<void> | void;
    submitLabel: string;
};

function GroupForm({initial, onSubmit, submitLabel}: GroupFormProps) {
    const isEdit = !!initial;
    const [name, setName] = useState(initial?.name ?? '');
    const [code, setCode] = useState(initial?.code ?? '');
    const [size, setSize] = useState(initial ? initial.size.toString() : '0');
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setSaving(true);
        setError(null);
        try {
            const body: any = {
                name: name.trim(),
                code: code.trim(),
                size: Number(size) || 0,
            };
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
                <label className="label">Название группы</label>
                <Input
                    value={name}
                    onChange={e => setName(e.currentTarget.value)}
                    required
                />
            </div>
            <div className="row">
                <label className="label">Код группы</label>
                <Input
                    value={code}
                    onChange={e => setCode(e.currentTarget.value)}
                    required
                />
            </div>
            <div className="row">
                <label className="label">Размер группы (кол-во студентов)</label>
                <Input
                    type="number"
                    min={0}
                    value={size}
                    onChange={e => setSize(e.currentTarget.value)}
                    required
                />
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

type SubgroupFormProps = {
    initial?: SubgroupDto;
    onSubmit: (data: CreateSubgroupReq | UpdateSubgroupReq) => Promise<void> | void;
    submitLabel: string;
};

function SubgroupForm({initial, onSubmit, submitLabel}: SubgroupFormProps) {
    const isEdit = !!initial;
    const [name, setName] = useState(initial?.name ?? '');
    const [size, setSize] = useState(
        initial ? initial.size.toString() : '0'
    );
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string | null>(null);

    const handleSubmit = async (e: React.FormEvent) => {
        e.preventDefault();
        setSaving(true);
        setError(null);
        try {
            const body: any = {
                name: name.trim(),
                size: Number(size) || 0,
            };
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
                <label className="label">Название подгруппы</label>
                <Input
                    value={name}
                    onChange={e => setName(e.currentTarget.value)}
                    required
                />
            </div>
            <div className="row">
                <label className="label">Размер (кол-во студентов)</label>
                <Input
                    type="number"
                    min={0}
                    value={size}
                    onChange={e => setSize(e.currentTarget.value)}
                    required
                />
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

function GroupsTable({
                         groups,
                         isAdmin,
                         onEditGroup,
                         onArchiveGroup,
                         onAddSubgroup,
                         onEditSubgroup,
                         onArchiveSubgroup,
                     }: {
    groups: GroupDto[];
    isAdmin: boolean;
    onEditGroup: (g: GroupDto) => void;
    onArchiveGroup: (g: GroupDto) => void;
    onAddSubgroup: (g: GroupDto) => void;
    onEditSubgroup: (g: GroupDto, s: SubgroupDto) => void;
    onArchiveSubgroup: (g: GroupDto, s: SubgroupDto) => void;
}) {
    return (
        <div className="card" style={{overflow: 'auto'}}>
            <table className="table">
                <thead>
                <tr>
                    <th>Код</th>
                    <th>Название</th>
                    <th>Размер</th> {/* <<< НОВЫЙ СТОЛБЕЦ */}
                    <th>Статус</th>
                    <th>Подгруппы</th>
                    <th>Создано</th>
                    <th>Обновлено</th>
                    {isAdmin && <th>Действия</th>}
                </tr>
                </thead>
                <tbody>
                {groups.map(g => (
                    <tr key={g.id}>
                        <td style={{fontWeight: 600}}>{g.code}</td>
                        <td>{g.name}</td>
                        <td>{g.size}</td>
                        <td>{g.status}</td>
                        <td>
                            {g.subgroups?.length ? (
                                <ul style={{paddingLeft: 16, margin: 0}}>
                                    {g.subgroups.map(s => (
                                        <li key={s.id}>
                                            {s.name} ({s.size}) [{s.status}]
                                            {isAdmin && (
                                                <>
                                                    {' '}
                                                    <Button
                                                        style={{marginLeft: 4}}
                                                        onClick={() => onEditSubgroup(g, s)}
                                                    >
                                                        ✎
                                                    </Button>
                                                    <Button
                                                        style={{marginLeft: 4}}
                                                        className="warn"
                                                        disabled={s.status === 'INACTIVE'}
                                                        onClick={() => onArchiveSubgroup(g, s)}
                                                    >
                                                        Архив
                                                    </Button>
                                                </>
                                            )}
                                        </li>
                                    ))}
                                </ul>
                            ) : (
                                <span>—</span>
                            )}
                        </td>
                        <td>{new Date(g.createdAt).toLocaleString()}</td>
                        <td>{new Date(g.updatedAt).toLocaleString()}</td>
                        {isAdmin && (
                            <td style={{textAlign: 'center'}}>
                                <div style={{display: 'flex', gap: 8, justifyContent: 'center', flexWrap: 'wrap'}}>
                                    <Button onClick={() => onEditGroup(g)}>Редактировать</Button>
                                    <Button onClick={() => onAddSubgroup(g)}>
                                        + подгруппа
                                    </Button>
                                    <Button
                                        className="warn"
                                        disabled={g.status === 'INACTIVE'}
                                        onClick={() => onArchiveGroup(g)}
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

export default function GroupsPage() {
    const {isAdmin} = useAuth();
    const [groups, setGroups] = useState<GroupDto[]>([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    const [showCreate, setShowCreate] = useState(false);
    const [editingGroup, setEditingGroup] = useState<GroupDto | null>(null);

    const [subgroupTarget, setSubgroupTarget] = useState<{group: GroupDto; subgroup?: SubgroupDto} | null>(null);

    const load = async () => {
        setLoading(true);
        setError(null);
        try {
            setGroups(await GroupsApi.list());
        } catch {
            setError('Ошибка загрузки групп');
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
                <div className="page-header-title">Группы</div>
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
                    <GroupsTable
                        groups={groups}
                        isAdmin={isAdmin}
                        onEditGroup={g => setEditingGroup(g)}
                        onArchiveGroup={async g => {
                            if (!window.confirm('Архивировать группу?')) return;
                            await GroupsApi.archive(g.id);
                            load();
                        }}
                        onAddSubgroup={g => setSubgroupTarget({group: g})}
                        onEditSubgroup={(g, s) => setSubgroupTarget({group: g, subgroup: s})}
                        onArchiveSubgroup={async (_g, s) => {
                            if (!window.confirm('Архивировать подгруппу?')) return;
                            await GroupsApi.archiveSubgroup(s.id);
                            load();
                        }}
                    />
                )}
            </main>

            {showCreate && (
                <Modal title="Создать группу" onClose={() => setShowCreate(false)}>
                    <GroupForm
                        submitLabel="Создать"
                        onSubmit={async data => {
                            await GroupsApi.create(data as CreateGroupReq);
                            setShowCreate(false);
                            load();
                        }}
                    />
                </Modal>
            )}

            {editingGroup && (
                <Modal
                    title={`Редактировать группу ${editingGroup.code}`}
                    onClose={() => setEditingGroup(null)}
                >
                    <GroupForm
                        initial={editingGroup}
                        submitLabel="Сохранить"
                        onSubmit={async data => {
                            await GroupsApi.update(editingGroup.id, data as UpdateGroupReq);
                            setEditingGroup(null);
                            load();
                        }}
                    />
                </Modal>
            )}

            {subgroupTarget && (
                <Modal
                    title={
                        subgroupTarget.subgroup
                            ? `Подгруппа ${subgroupTarget.subgroup.name}`
                            : `Новая подгруппа для ${subgroupTarget.group.code}`
                    }
                    onClose={() => setSubgroupTarget(null)}
                >
                    <SubgroupForm
                        initial={subgroupTarget.subgroup}
                        submitLabel={subgroupTarget.subgroup ? 'Сохранить' : 'Создать'}
                        onSubmit={async data => {
                            if (subgroupTarget.subgroup) {
                                await GroupsApi.updateSubgroup(
                                    subgroupTarget.subgroup.id,
                                    data as UpdateSubgroupReq
                                );
                            } else {
                                await GroupsApi.createSubgroup(
                                    subgroupTarget.group.id,
                                    data as CreateSubgroupReq
                                );
                            }
                            setSubgroupTarget(null);
                            load();
                        }}
                    />
                </Modal>
            )}
        </div>
    );
}
