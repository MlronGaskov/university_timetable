CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS teachers
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version    BIGINT       NOT NULL DEFAULT 0,
    teacher_id VARCHAR(64)  NOT NULL,
    full_name  VARCHAR(128) NOT NULL,
    status     VARCHAR(16)  NOT NULL,
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now(),

    CONSTRAINT uk_teachers_full_name UNIQUE (full_name),
    CONSTRAINT uk_teachers_teacher_id UNIQUE (teacher_id)
);

CREATE TABLE IF NOT EXISTS students
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version    BIGINT       NOT NULL DEFAULT 0,
    full_name  VARCHAR(128) NOT NULL,
    student_id VARCHAR(64)  NOT NULL,
    status     VARCHAR(16)  NOT NULL,
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CONSTRAINT uk_students_student_id UNIQUE (student_id)
);

CREATE TABLE IF NOT EXISTS groups
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version    BIGINT       NOT NULL DEFAULT 0,
    name       VARCHAR(128) NOT NULL,
    code       VARCHAR(64)  NOT NULL,
    size       INTEGER      NOT NULL,
    status     VARCHAR(16)  NOT NULL,
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CONSTRAINT uk_groups_code UNIQUE (code)
);

CREATE TABLE IF NOT EXISTS group_students
(
    group_id   UUID        NOT NULL,
    student_id VARCHAR(64) NOT NULL,
    PRIMARY KEY (group_id, student_id),
    CONSTRAINT fk_group_students_group
        FOREIGN KEY (group_id) REFERENCES groups (id) ON DELETE CASCADE,
    CONSTRAINT fk_group_students_student
        FOREIGN KEY (student_id) REFERENCES students (student_id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS rooms
(
    id         UUID PRIMARY KEY     DEFAULT gen_random_uuid(),
    version    BIGINT      NOT NULL DEFAULT 0,
    code       VARCHAR(64) NOT NULL,
    building   VARCHAR(64) NOT NULL,
    number     VARCHAR(64) NOT NULL,
    capacity   INTEGER     NOT NULL,
    status     VARCHAR(16) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT uk_rooms_building_number UNIQUE (building, number),
    CONSTRAINT uk_rooms_code UNIQUE (code)
);

CREATE TABLE IF NOT EXISTS room_items
(
    room_id   UUID         NOT NULL,
    item_name VARCHAR(128) NOT NULL,
    item_qty  INTEGER      NOT NULL,
    PRIMARY KEY (room_id, item_name),
    CONSTRAINT fk_room_items_room
        FOREIGN KEY (room_id) REFERENCES rooms (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS policies
(
    id                 UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version            BIGINT       NOT NULL DEFAULT 0,
    name               VARCHAR(128) NOT NULL,
    grid_json          TEXT         NOT NULL,
    breaks_json        TEXT         NOT NULL,
    limits_json        TEXT         NOT NULL,
    travel_matrix_json TEXT         NOT NULL,
    weights_json       TEXT         NOT NULL,
    created_at         TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at         TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CONSTRAINT uk_policies_name UNIQUE (name)
);

CREATE TABLE IF NOT EXISTS users
(
    id            UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version       BIGINT       NOT NULL DEFAULT 0,
    login         VARCHAR(64)  NOT NULL,
    email         VARCHAR(128) NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    role          VARCHAR(16)  NOT NULL,
    status        VARCHAR(16)  NOT NULL,
    teacher_id    UUID,
    student_id    UUID,
    created_at    TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at    TIMESTAMPTZ  NOT NULL DEFAULT now(),

    CONSTRAINT uk_users_login UNIQUE (login),
    CONSTRAINT uk_users_email UNIQUE (email),
    CONSTRAINT uk_users_teacher UNIQUE (teacher_id),
    CONSTRAINT uk_users_student UNIQUE (student_id),

    CONSTRAINT fk_users_teacher
        FOREIGN KEY (teacher_id) REFERENCES teachers (id),
    CONSTRAINT fk_users_student
        FOREIGN KEY (student_id) REFERENCES students (id)
);

CREATE TABLE IF NOT EXISTS teacher_working_hours
(
    teacher_id_fk UUID        NOT NULL,
    day_of_week   VARCHAR(10) NOT NULL,
    start_time    TIME        NOT NULL,
    end_time      TIME        NOT NULL,
    PRIMARY KEY (teacher_id_fk, day_of_week, start_time),
    CONSTRAINT fk_teacher_working_hours_teacher
        FOREIGN KEY (teacher_id_fk) REFERENCES teachers (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS courses
(
    id                     UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version                BIGINT       NOT NULL DEFAULT 0,
    code                   VARCHAR(64)  NOT NULL,
    title                  VARCHAR(256) NOT NULL,
    teacher_id             VARCHAR(64)  NOT NULL,
    planned_hours          INTEGER      NOT NULL,
    required_room_capacity INTEGER      NOT NULL DEFAULT 0,
    status                 VARCHAR(16)  NOT NULL,
    created_at             TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at             TIMESTAMPTZ  NOT NULL DEFAULT now(),

    CONSTRAINT uk_courses_code UNIQUE (code),

    CONSTRAINT fk_courses_teacher
        FOREIGN KEY (teacher_id) REFERENCES teachers (teacher_id) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS course_groups
(
    course_id  UUID        NOT NULL,
    group_code VARCHAR(64) NOT NULL,
    PRIMARY KEY (course_id, group_code),
    CONSTRAINT fk_course_groups_course
        FOREIGN KEY (course_id) REFERENCES courses (id) ON DELETE CASCADE,
    CONSTRAINT fk_course_groups_group
        FOREIGN KEY (group_code) REFERENCES groups (code) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS course_items
(
    course_id UUID         NOT NULL,
    item_name VARCHAR(128) NOT NULL,
    item_qty  INTEGER      NOT NULL,
    PRIMARY KEY (course_id, item_name),
    CONSTRAINT fk_course_items_course
        FOREIGN KEY (course_id) REFERENCES courses (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS semesters
(
    id          UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    version     BIGINT       NOT NULL DEFAULT 0,
    code        VARCHAR(64)  NOT NULL,
    start_at    TIMESTAMPTZ  NOT NULL,
    end_at      TIMESTAMPTZ  NOT NULL,
    policy_name VARCHAR(128) NOT NULL,
    status      VARCHAR(16)  NOT NULL,
    created_at  TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at  TIMESTAMPTZ  NOT NULL DEFAULT now(),

    CONSTRAINT uk_semesters_code UNIQUE (code),
    CONSTRAINT fk_semesters_policy_name
        FOREIGN KEY (policy_name) REFERENCES policies (name) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS semester_rooms
(
    semester_id UUID        NOT NULL,
    room_code   VARCHAR(64) NOT NULL,
    PRIMARY KEY (semester_id, room_code),
    CONSTRAINT fk_semester_rooms_semester
        FOREIGN KEY (semester_id) REFERENCES semesters (id) ON DELETE CASCADE,
    CONSTRAINT fk_semester_rooms_room
        FOREIGN KEY (room_code) REFERENCES rooms (code) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS semester_courses
(
    semester_id UUID        NOT NULL,
    course_code VARCHAR(64) NOT NULL,
    PRIMARY KEY (semester_id, course_code),
    CONSTRAINT fk_semester_courses_semester
        FOREIGN KEY (semester_id) REFERENCES semesters (id) ON DELETE CASCADE,
    CONSTRAINT fk_semester_courses_course
        FOREIGN KEY (course_code) REFERENCES courses (code) ON UPDATE CASCADE
);

CREATE TABLE IF NOT EXISTS schedules
(
    id                UUID PRIMARY KEY     DEFAULT gen_random_uuid(),
    semester_code     VARCHAR(64) NOT NULL,
    version           INTEGER     NOT NULL,
    evaluation_score  DOUBLE PRECISION,
    status            VARCHAR(16) NOT NULL DEFAULT 'ACTIVE',
    base_schedule_id  UUID,
    generation_reason VARCHAR(64),
    created_at        TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at        TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT fk_schedules_semester
        FOREIGN KEY (semester_code) REFERENCES semesters (code)
            ON UPDATE CASCADE
            ON DELETE CASCADE,

    CONSTRAINT fk_schedules_base_schedule
        FOREIGN KEY (base_schedule_id) REFERENCES schedules (id),

    CONSTRAINT uk_schedules_semester_version
        UNIQUE (semester_code, version)
);

CREATE UNIQUE INDEX uk_one_active_schedule_per_semester
    ON schedules (semester_code)
    WHERE status = 'ACTIVE';

CREATE TABLE IF NOT EXISTS schedule_slots
(
    id           UUID PRIMARY KEY     DEFAULT gen_random_uuid(),
    schedule_id  UUID        NOT NULL,
    course_code  VARCHAR(64) NOT NULL,
    room_code    VARCHAR(64) NOT NULL,
    day_of_week  VARCHAR(16) NOT NULL,
    start_time   TIME        NOT NULL,
    end_time     TIME        NOT NULL,
    valid_from   DATE        NOT NULL,
    valid_until  DATE        NOT NULL,
    week_pattern VARCHAR(16) NOT NULL,
    created_at   TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at   TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT fk_schedule_slots_schedule
        FOREIGN KEY (schedule_id) REFERENCES schedules (id) ON DELETE CASCADE,
    CONSTRAINT fk_schedule_slots_course
        FOREIGN KEY (course_code) REFERENCES courses (code) ON UPDATE CASCADE,
    CONSTRAINT fk_schedule_slots_room
        FOREIGN KEY (room_code) REFERENCES rooms (code) ON UPDATE CASCADE
);

CREATE TABLE schedule_generation_locks
(
    semester_code VARCHAR(64) PRIMARY KEY,
    version       BIGINT      NOT NULL DEFAULT 0,
    updated_at    TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT fk_schedule_generation_locks_semester
        FOREIGN KEY (semester_code) REFERENCES semesters (code)
            ON UPDATE CASCADE
            ON DELETE CASCADE
);

CREATE INDEX idx_courses_teacher_id ON courses (teacher_id);
CREATE INDEX idx_course_groups_group_code ON course_groups (group_code);
CREATE INDEX idx_semester_courses_course_code ON semester_courses (course_code);
CREATE INDEX idx_semester_rooms_room_code ON semester_rooms (room_code);
CREATE INDEX idx_schedules_semester_code ON schedules (semester_code);
CREATE INDEX idx_schedule_slots_schedule_id ON schedule_slots (schedule_id);
CREATE INDEX idx_schedule_slots_course_code ON schedule_slots (course_code);
CREATE INDEX idx_schedule_slots_room_code ON schedule_slots (room_code);
