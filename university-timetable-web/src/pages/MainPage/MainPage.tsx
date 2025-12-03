import React from "react";
import {useAuth} from "@hooks/useAuth";

export const MainPage: React.FC = () => {
    const {role} = useAuth();

    return (
        <div className="Page-root">
            <div className="Page-card">
                <div className="Page-header">
                    <div>
                        <h1 className="Page-headerTitle">Главная</h1>
                        <p className="Page-headerSubtitle">
                            Быстрый доступ к расписанию и управлению учётной записью.
                        </p>
                    </div>
                </div>

                <div className="MainPage-grid">
                    <div className="MainPage-card">
                        <h2>Сегодня</h2>
                        <p>Здесь можно будет вывести пары/занятия на сегодня.</p>
                    </div>

                    <div className="MainPage-card">
                        <h2>Моё расписание</h2>
                        <p>Перейдите на вкладку &laquo;Расписание&raquo;, чтобы увидеть
                            все предстоящие занятия.</p>
                    </div>

                    <div className="MainPage-card">
                        <h2>Профиль</h2>
                        <p>В профиле доступны ваши данные и настройки.</p>
                    </div>

                    {role === "ADMIN" && (
                        <div className="MainPage-card MainPage-cardAdmin">
                            <h2>Администрирование</h2>
                            <p>
                                Как администратор вы можете управлять пользователями,
                                создавать и редактировать учётные записи.
                            </p>
                        </div>
                    )}
                </div>
            </div>
        </div>
    );
};
