-- Версии расписания (UC-SCHED-1,5)
CREATE TABLE IF NOT EXISTS schedule_versions (
    id              uuid PRIMARY KEY,
    name            varchar(255)          NOT NULL,

    semester_id     uuid                  NOT NULL,
    status          varchar(32)           NOT NULL, -- DRAFT / ACTIVE / ARCHIVED
    published       boolean               NOT NULL DEFAULT false,

    -- Метрики качества
    total_score         double precision,
    gaps_score          double precision,
    load_balance_score  double precision,
    preferences_score   double precision,

    created_at      timestamptz           NOT NULL DEFAULT now(),
    updated_at      timestamptz
);

-- для таблицы семестров
-- ALTER TABLE schedule_versions
--     ADD CONSTRAINT fk_schedule_versions_semester
--         FOREIGN KEY (semester_id) REFERENCES semesters(id);


-- Занятия в расписании (UC-SCHED-1,2,3,4)
CREATE TABLE IF NOT EXISTS class_sessions (
    id              uuid PRIMARY KEY,

    version_id      uuid                  NOT NULL,
    course_id       uuid                  NOT NULL,
    teacher_id      uuid                  NOT NULL,
    "group_id"      uuid                  NOT NULL,
    subgroup_id     uuid,
    room_id         uuid                  NOT NULL,

    "date"          date                  NOT NULL,
    slot_index      integer               NOT NULL,
    duration_slots  integer               NOT NULL DEFAULT 1,

    locked          boolean               NOT NULL DEFAULT false
);

ALTER TABLE class_sessions
    ADD CONSTRAINT fk_class_sessions_version
        FOREIGN KEY (version_id) REFERENCES schedule_versions(id)
        ON DELETE CASCADE;

-- пока нет соотв таблиц
-- ALTER TABLE class_sessions
--     ADD CONSTRAINT fk_class_sessions_course
--         FOREIGN KEY (course_id) REFERENCES courses(id);
--
-- ALTER TABLE class_sessions
--     ADD CONSTRAINT fk_class_sessions_teacher
--         FOREIGN KEY (teacher_id) REFERENCES teachers(id);
--
-- ALTER TABLE class_sessions
--     ADD CONSTRAINT fk_class_sessions_group
--         FOREIGN KEY ("group_id") REFERENCES groups(id);
--
-- ALTER TABLE class_sessions
--     ADD CONSTRAINT fk_class_sessions_subgroup
--         FOREIGN KEY (subgroup_id) REFERENCES subgroups(id);
--
-- ALTER TABLE class_sessions
--     ADD CONSTRAINT fk_class_sessions_room
--         FOREIGN KEY (room_id) REFERENCES rooms(id);


-- Индексы под типичные запросы (UC-SCHED-2,4)
CREATE INDEX IF NOT EXISTS idx_class_sessions_version
    ON class_sessions (version_id);

CREATE INDEX IF NOT EXISTS idx_class_sessions_teacher_time
    ON class_sessions (teacher_id, "date", slot_index);

CREATE INDEX IF NOT EXISTS idx_class_sessions_group_time
    ON class_sessions ("group_id", "date", slot_index);

CREATE INDEX IF NOT EXISTS idx_class_sessions_room_time
    ON class_sessions (room_id, "date", slot_index);


-- Локи версий расписаний (UC-SCHED-3)
CREATE TABLE IF NOT EXISTS schedule_locks (
    version_id      uuid PRIMARY KEY,
    owner_username  varchar(255)          NOT NULL,
    acquired_at     timestamptz           NOT NULL,
    expires_at      timestamptz           NOT NULL
);

ALTER TABLE schedule_locks
    ADD CONSTRAINT fk_schedule_locks_version
        FOREIGN KEY (version_id) REFERENCES schedule_versions(id)
        ON DELETE CASCADE;
