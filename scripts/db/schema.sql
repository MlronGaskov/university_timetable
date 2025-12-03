CREATE EXTENSION IF NOT EXISTS pgcrypto;

CREATE TABLE IF NOT EXISTS teachers
(
    id         UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
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
    teacher_id_fk UUID     NOT NULL,
    day_of_week   SMALLINT NOT NULL,
    start_time    TIME     NOT NULL,
    end_time      TIME     NOT NULL,
    PRIMARY KEY (teacher_id_fk, day_of_week, start_time),
    CONSTRAINT fk_teacher_working_hours_teacher
        FOREIGN KEY (teacher_id_fk) REFERENCES teachers (id) ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS courses
(
    id            UUID PRIMARY KEY      DEFAULT gen_random_uuid(),
    code          VARCHAR(64)  NOT NULL,
    title         VARCHAR(256) NOT NULL,
    teacher_id    VARCHAR(64)  NOT NULL,
    planned_hours INTEGER      NOT NULL,
    status        VARCHAR(16)  NOT NULL,
    created_at    TIMESTAMPTZ  NOT NULL DEFAULT now(),
    updated_at    TIMESTAMPTZ  NOT NULL DEFAULT now(),

    CONSTRAINT uk_courses_code UNIQUE (code),

    CONSTRAINT fk_courses_teacher
        FOREIGN KEY (teacher_id) REFERENCES teachers (teacher_id)
);

CREATE TABLE IF NOT EXISTS course_groups
(
    course_id UUID NOT NULL,
    group_id  UUID NOT NULL,
    PRIMARY KEY (course_id, group_id),
    CONSTRAINT fk_course_groups_course
        FOREIGN KEY (course_id) REFERENCES courses (id) ON DELETE CASCADE,
    CONSTRAINT fk_course_groups_group
        FOREIGN KEY (group_id) REFERENCES groups (id)
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
    id         UUID PRIMARY KEY     DEFAULT gen_random_uuid(),
    code       VARCHAR(64) NOT NULL,
    start_at   TIMESTAMPTZ NOT NULL,
    end_at     TIMESTAMPTZ NOT NULL,
    policy_id  UUID        NOT NULL,
    status     VARCHAR(16) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),

    CONSTRAINT uk_semesters_code UNIQUE (code),
    CONSTRAINT fk_semesters_policy
        FOREIGN KEY (policy_id) REFERENCES policies (id)
);

CREATE TABLE IF NOT EXISTS semester_rooms
(
    semester_id UUID        NOT NULL,
    room_code   VARCHAR(64) NOT NULL,
    PRIMARY KEY (semester_id, room_code),
    CONSTRAINT fk_semester_rooms_semester
        FOREIGN KEY (semester_id) REFERENCES semesters (id) ON DELETE CASCADE,
    CONSTRAINT fk_semester_rooms_room
        FOREIGN KEY (room_code) REFERENCES rooms (code)
);

CREATE TABLE IF NOT EXISTS semester_courses
(
    semester_id UUID NOT NULL,
    course_id   UUID NOT NULL,
    PRIMARY KEY (semester_id, course_id),
    CONSTRAINT fk_semester_courses_semester
        FOREIGN KEY (semester_id) REFERENCES semesters (id) ON DELETE CASCADE,
    CONSTRAINT fk_semester_courses_course
        FOREIGN KEY (course_id) REFERENCES courses (id)
);
