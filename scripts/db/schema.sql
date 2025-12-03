CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS teachers
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    full_name  VARCHAR(128) NOT NULL,
    status     VARCHAR(16)  NOT NULL,
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    CONSTRAINT uk_teachers_full_name UNIQUE (full_name)
);

CREATE TABLE IF NOT EXISTS students
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
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
    group_id   UUID NOT NULL,
    student_id UUID NOT NULL,
    PRIMARY KEY (group_id, student_id),
    CONSTRAINT fk_group_students_group
        FOREIGN KEY (group_id) REFERENCES groups (id) ON DELETE CASCADE,
    CONSTRAINT fk_group_students_student
        FOREIGN KEY (student_id) REFERENCES students (id)
);

CREATE TABLE IF NOT EXISTS rooms
(
    id         UUID PRIMARY KEY     DEFAULT gen_random_uuid(),
    building   VARCHAR(64) NOT NULL,
    number     VARCHAR(64) NOT NULL,
    capacity   INTEGER     NOT NULL,
    status     VARCHAR(16) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    CONSTRAINT uk_rooms_building_number UNIQUE (building, number)
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
    teacher_id  UUID     NOT NULL,
    day_of_week SMALLINT NOT NULL,
    start_time  TIME     NOT NULL,
    end_time    TIME     NOT NULL,
    PRIMARY KEY (teacher_id, day_of_week, start_time),
    CONSTRAINT fk_teacher_working_hours_teacher
        FOREIGN KEY (teacher_id) REFERENCES teachers (id) ON DELETE CASCADE
);
