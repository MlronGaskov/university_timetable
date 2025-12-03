import React, {useEffect, useState} from "react";
import {useAuth} from "@hooks/useAuth";
import {usersApi} from "@/api/users";
import {teachersApi} from "@/api/teachers";
import {studentsApi} from "@/api/students";
import type {UserResponse} from "@/types/auth";
import type {
    TeacherResponse,
    WorkingIntervalDto,
    UpdateWorkingHoursRequest
} from "@/types/teacher";
import type {StudentResponse} from "@/types/student";
import "./ProfilePage.css";

const DAY_OPTIONS: WorkingIntervalDto["day"][] = [
    "MONDAY",
    "TUESDAY",
    "WEDNESDAY",
    "THURSDAY",
    "FRIDAY",
    "SATURDAY",
];

export const ProfilePage: React.FC = () => {
    const {userId, teacherId, studentId, role} = useAuth();

    const [user, setUser] = useState<UserResponse | null>(null);
    const [teacher, setTeacher] = useState<TeacherResponse | null>(null);
    const [student, setStudent] = useState<StudentResponse | null>(null);

    const [loading, setLoading] = useState(true);
    const [error, setError] = useState<string | null>(null);

    const [intervals, setIntervals] = useState<WorkingIntervalDto[]>([]);
    const [editIntervalsMode, setEditIntervalsMode] = useState(false);
    const [savingIntervals, setSavingIntervals] = useState(false);
    const [intervalsError, setIntervalsError] = useState<string | null>(null);

    useEffect(() => {
        let cancelled = false;

        const load = async () => {
            setLoading(true);
            setError(null);

            try {
                const promises: Promise<void>[] = [];

                if (!userId && !teacherId && !studentId) {
                    setError(
                        "Не удалось определить профиль пользователя: в токене нет идентификаторов."
                    );
                    setLoading(false);
                    return;
                }

                if (userId) {
                    promises.push(
                        usersApi
                            .getOne(userId as any)
                            .then(u => {
                                if (!cancelled) setUser(u);
                            })
                            .catch(() => {
                            })
                    );
                }

                if (teacherId) {
                    promises.push(
                        teachersApi
                            .getOne(teacherId as any)
                            .then(t => {
                                if (!cancelled) {
                                    setTeacher(t);
                                    setIntervals(t.preferredWorkingHours ?? []);
                                }
                            })
                            .catch(() => {
                            })
                    );
                }

                if (studentId) {
                    promises.push(
                        studentsApi
                            .getOne(studentId as any)
                            .then(s => {
                                if (!cancelled) setStudent(s);
                            })
                            .catch(() => {
                            })
                    );
                }

                if (promises.length > 0) {
                    await Promise.all(promises);
                }
            } catch {
                if (!cancelled) {
                    setError("Не удалось загрузить данные профиля");
                }
            } finally {
                if (!cancelled) setLoading(false);
            }
        };

        void load();

        return () => {
            cancelled = true;
        };
    }, [userId, teacherId, studentId]);

    const renderTeacherIntervalsList = (t: TeacherResponse | null) => {
        if (!t || !t.preferredWorkingHours || t.preferredWorkingHours.length === 0) {
            return <span>—</span>;
        }

        return (
            <ul style={{margin: 0, paddingLeft: 16, fontSize: 13}}>
                {t.preferredWorkingHours.map((h, idx) => (
                    <li key={`${h.day}-${h.startTime}-${h.endTime}-${idx}`}>
                        {h.day} {h.startTime}–{h.endTime}
                    </li>
                ))}
            </ul>
        );
    };

    const handleIntervalChange = (
        index: number,
        patch: Partial<WorkingIntervalDto>
    ) => {
        setIntervals(prev =>
            prev.map((it, i) => (i === index ? {...it, ...patch} : it))
        );
    };

    const handleAddInterval = () => {
        setIntervals(prev => [
            ...prev,
            {day: "MONDAY", startTime: "09:00", endTime: "10:30"},
        ]);
    };

    const handleRemoveInterval = (index: number) => {
        setIntervals(prev => prev.filter((_, i) => i !== index));
    };

    const saveIntervals = async () => {
        if (!teacher || !teacherId) return;
        setSavingIntervals(true);
        setIntervalsError(null);

        try {
            const payload: UpdateWorkingHoursRequest = {
                preferredWorkingHours: intervals,
            };
            const updated = await teachersApi.updateWorkingHours(
                teacher.id as any,
                payload
            );
            setTeacher(updated);
            setIntervals(updated.preferredWorkingHours ?? []);
            setEditIntervalsMode(false);
        } catch {
            setIntervalsError("Не удалось сохранить рабочие интервалы");
        } finally {
            setSavingIntervals(false);
        }
    };

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Профиль</h1>
                        <p className="Page-headerSubtitle">
                            Ваши данные, роль в системе и связанные сущности (преподаватель / студент).
                        </p>
                    </div>
                </div>

                {loading && <div>Загрузка...</div>}
                {error && <div className="Profile-error">{error}</div>}

                {!loading && !error && (
                    <div className="Profile-grid">
                        <div className="Profile-block">
                            <h2>Учётная запись</h2>
                            <div className="Profile-row">
                                <span>UID пользователя</span>
                                <strong>{user?.id ?? userId ?? "—"}</strong>
                            </div>
                            <div className="Profile-row">
                                <span>Роль (из токена)</span>
                                <strong>{role ?? "—"}</strong>
                            </div>
                            <div className="Profile-row">
                                <span>Логин</span>
                                <strong>{user?.login ?? "—"}</strong>
                            </div>
                            <div className="Profile-row">
                                <span>Email</span>
                                <strong>{user?.email ?? "—"}</strong>
                            </div>
                            <div className="Profile-row">
                                <span>Статус</span>
                                <strong>{user?.status ?? "—"}</strong>
                            </div>
                            <div className="Profile-row">
                                <span>teacherId (связь)</span>
                                <strong>{user?.teacherId ?? teacherId ?? "—"}</strong>
                            </div>
                            <div className="Profile-row">
                                <span>studentId (связь)</span>
                                <strong>{user?.studentId ?? studentId ?? "—"}</strong>
                            </div>
                            {user && (
                                <>
                                    <div className="Profile-row">
                                        <span>Создан</span>
                                        <strong>
                                            {new Date(user.createdAt).toLocaleString()}
                                        </strong>
                                    </div>
                                    <div className="Profile-row">
                                        <span>Обновлён</span>
                                        <strong>
                                            {new Date(user.updatedAt).toLocaleString()}
                                        </strong>
                                    </div>
                                </>
                            )}
                        </div>

                        {teacher && (
                            <div className="Profile-block">
                                <h2>Преподаватель</h2>
                                <div className="Profile-row">
                                    <span>UID преподавателя</span>
                                    <strong>{teacher.id}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>ФИО</span>
                                    <strong>{teacher.fullName}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>Статус</span>
                                    <strong>{teacher.status}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>Создан</span>
                                    <strong>
                                        {new Date(teacher.createdAt).toLocaleString()}
                                    </strong>
                                </div>
                                <div className="Profile-row">
                                    <span>Обновлён</span>
                                    <strong>
                                        {new Date(teacher.updatedAt).toLocaleString()}
                                    </strong>
                                </div>

                                <div style={{marginTop: 8}}>
                                    <div
                                        style={{
                                            fontSize: 13,
                                            color: "#6b7280",
                                            marginBottom: 4,
                                        }}
                                    >
                                        Рабочие интервалы
                                    </div>

                                    {!editIntervalsMode && (
                                        <>
                                            {renderTeacherIntervalsList(teacher)}
                                            {role === "TEACHER" && (
                                                <button
                                                    type="button"
                                                    className="SecondaryButton"
                                                    style={{marginTop: 6}}
                                                    onClick={() => {
                                                        setIntervals(
                                                            teacher.preferredWorkingHours ?? []
                                                        );
                                                        setIntervalsError(null);
                                                        setEditIntervalsMode(true);
                                                    }}
                                                >
                                                    Настроить интервалы
                                                </button>
                                            )}
                                        </>
                                    )}

                                    {editIntervalsMode && role === "TEACHER" && (
                                        <>
                                            {intervals.map((it, idx) => (
                                                <div
                                                    key={`${it.day}-${it.startTime}-${it.endTime}-${idx}`}
                                                    style={{
                                                        display: "flex",
                                                        flexWrap: "wrap",
                                                        gap: 8,
                                                        marginBottom: 4,
                                                        alignItems: "center",
                                                    }}
                                                >
                                                    <select
                                                        value={it.day}
                                                        onChange={e =>
                                                            handleIntervalChange(idx, {
                                                                day: e.target
                                                                    .value as WorkingIntervalDto["day"],
                                                            })
                                                        }
                                                    >
                                                        {DAY_OPTIONS.map(d => (
                                                            <option key={d} value={d}>
                                                                {d}
                                                            </option>
                                                        ))}
                                                    </select>
                                                    <input
                                                        type="time"
                                                        value={it.startTime}
                                                        onChange={e =>
                                                            handleIntervalChange(idx, {
                                                                startTime: e.target.value,
                                                            })
                                                        }
                                                        required
                                                    />
                                                    <input
                                                        type="time"
                                                        value={it.endTime}
                                                        onChange={e =>
                                                            handleIntervalChange(idx, {
                                                                endTime: e.target.value,
                                                            })
                                                        }
                                                        required
                                                    />
                                                    <button
                                                        type="button"
                                                        className="SecondaryButton"
                                                        onClick={() => handleRemoveInterval(idx)}
                                                    >
                                                        Удалить
                                                    </button>
                                                </div>
                                            ))}

                                            <button
                                                type="button"
                                                className="SecondaryButton"
                                                onClick={handleAddInterval}
                                                style={{marginTop: 4}}
                                            >
                                                + Добавить интервал
                                            </button>

                                            <div
                                                className="FormActions"
                                                style={{
                                                    marginTop: 8,
                                                    justifyContent: "flex-start",
                                                }}
                                            >
                                                <button
                                                    type="button"
                                                    className="SecondaryButton"
                                                    onClick={() => {
                                                        setEditIntervalsMode(false);
                                                        setIntervals(
                                                            teacher.preferredWorkingHours ?? []
                                                        );
                                                        setIntervalsError(null);
                                                    }}
                                                >
                                                    Отмена
                                                </button>
                                                <button
                                                    type="button"
                                                    className="PrimaryButton"
                                                    disabled={savingIntervals}
                                                    onClick={saveIntervals}
                                                >
                                                    {savingIntervals
                                                        ? "Сохранение..."
                                                        : "Сохранить"}
                                                </button>
                                            </div>

                                            {intervalsError && (
                                                <div
                                                    className="Profile-error"
                                                    style={{marginTop: 6}}
                                                >
                                                    {intervalsError}
                                                </div>
                                            )}
                                        </>
                                    )}
                                </div>
                            </div>
                        )}

                        {student && (
                            <div className="Profile-block">
                                <h2>Студент</h2>
                                <div className="Profile-row">
                                    <span>UID студента</span>
                                    <strong>{student.id}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>ФИО</span>
                                    <strong>{student.fullName}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>studentId</span>
                                    <strong>{student.studentId}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>Статус</span>
                                    <strong>{student.status}</strong>
                                </div>
                                <div className="Profile-row">
                                    <span>Создан</span>
                                    <strong>
                                        {new Date(student.createdAt).toLocaleString()}
                                    </strong>
                                </div>
                                <div className="Profile-row">
                                    <span>Обновлён</span>
                                    <strong>
                                        {new Date(student.updatedAt).toLocaleString()}
                                    </strong>
                                </div>
                            </div>
                        )}
                    </div>
                )}
            </div>
        </div>
    );
};
