import React from 'react';
import './TimetablePage.css';

interface Lesson {
    id: number;
    time: string;
    subject: string;
    room: string;
    teacher: string;
    group?: string;
}

const mockLessons: Lesson[] = [
    {
        id: 1,
        time: '08:30 – 10:00',
        subject: 'Алгоритмы и структуры данных',
        room: 'Ауд. 201',
        teacher: 'Иванов И.И.',
        group: 'IT-101',
    },
    {
        id: 2,
        time: '10:15 – 11:45',
        subject: 'Операционные системы',
        room: 'Лаб. 3',
        teacher: 'Петров П.П.',
        group: 'IT-101',
    },
];

export const TimetablePage: React.FC = () => {
    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Расписание</h1>
                        <p className="Page-headerSubtitle">
                            Пока пример отображения расписания на день. Позже можно
                            привязать к бэку.
                        </p>
                    </div>
                </div>

                <div className="Timetable-filters">
                    <select>
                        <option>Сегодня</option>
                        <option>Завтра</option>
                        <option>Эта неделя</option>
                    </select>
                    <select>
                        <option>Моя группа</option>
                    </select>
                </div>

                <div className="Timetable-list">
                    {mockLessons.map(lesson => (
                        <div className="Timetable-item" key={lesson.id}>
                            <div className="Timetable-time">{lesson.time}</div>
                            <div className="Timetable-main">
                                <div className="Timetable-subject">{lesson.subject}</div>
                                <div className="Timetable-meta">
                                    <span>{lesson.room}</span>
                                    <span>{lesson.teacher}</span>
                                    {lesson.group && <span>{lesson.group}</span>}
                                </div>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    );
};
