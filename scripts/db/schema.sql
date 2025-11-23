CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS users
(
    id            UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    login         VARCHAR(64)  NOT NULL UNIQUE,
    email         VARCHAR(255) NOT NULL UNIQUE,
    password_hash TEXT         NOT NULL,
    role          VARCHAR(32)  NOT NULL,
    status        VARCHAR(32)  NOT NULL,

    created_at    TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at    TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS groups
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    name       VARCHAR(255) NOT NULL,
    code       VARCHAR(64)  NOT NULL UNIQUE,
    size       INTEGER      NOT NULL CHECK (size >= 0),
    status     VARCHAR(32)  NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS subgroups
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    group_id   UUID         NOT NULL REFERENCES groups (id) ON DELETE CASCADE,
    name       VARCHAR(255) NOT NULL,
    size       INTEGER      NOT NULL CHECK (size >= 0),
    status     VARCHAR(32)  NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now(),

    CONSTRAINT uq_subgroup_name_per_group UNIQUE (group_id, name)
);

CREATE TABLE IF NOT EXISTS students
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    full_name  VARCHAR(255) NOT NULL,
    student_id VARCHAR(64)  NOT NULL UNIQUE,
    group_id   UUID         REFERENCES groups (id) ON DELETE SET NULL,
    status     VARCHAR(32)  NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS teachers
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    full_name  VARCHAR(255) NOT NULL,
    status     VARCHAR(32)  NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ  NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS rooms
(
    id         UUID PRIMARY KEY     DEFAULT gen_random_uuid(),
    building   VARCHAR(64) NOT NULL,
    number     VARCHAR(64) NOT NULL,
    capacity   INTEGER     NOT NULL CHECK (capacity >= 0),

    equipment  TEXT,

    status     VARCHAR(32) NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT uq_room_building_number UNIQUE (building, number)
);

CREATE TABLE IF NOT EXISTS room_equipment
(
    id       UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    room_id  UUID         NOT NULL REFERENCES rooms (id) ON DELETE CASCADE,
    name     VARCHAR(255) NOT NULL,
    quantity INTEGER      NOT NULL DEFAULT 1 CHECK (quantity >= 0),

    CONSTRAINT uq_room_equipment UNIQUE (room_id, name)
);
