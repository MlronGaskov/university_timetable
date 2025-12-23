COPY teachers (teacher_id, full_name, status)
    FROM '/docker-entrypoint-initdb.d/csv/teachers.csv'
    DELIMITER ',' CSV HEADER;

COPY groups (name, code, size, status)
    FROM '/docker-entrypoint-initdb.d/csv/groups.csv'
    DELIMITER ',' CSV HEADER;

COPY rooms (code, building, number, capacity, status)
    FROM '/docker-entrypoint-initdb.d/csv/rooms.csv'
    DELIMITER ',' CSV HEADER;

COPY students (full_name, student_id, status)
    FROM '/docker-entrypoint-initdb.d/csv/students.csv'
    DELIMITER ',' CSV HEADER;

COPY policies (name, grid_json, breaks_json, limits_json, travel_matrix_json, weights_json)
    FROM '/docker-entrypoint-initdb.d/csv/policies.csv'
    DELIMITER ',' CSV HEADER;

COPY courses (code, title, teacher_id, planned_hours, required_room_capacity, status)
    FROM '/docker-entrypoint-initdb.d/csv/courses.csv'
    DELIMITER ',' CSV HEADER;

COPY semesters (code, start_at, end_at, policy_name, status)
    FROM '/docker-entrypoint-initdb.d/csv/semesters.csv'
    DELIMITER ',' CSV HEADER;

CREATE TEMP TABLE tmp_group_students
(
    group_code VARCHAR(64),
    student_id VARCHAR(64)
);

COPY tmp_group_students (group_code, student_id)
    FROM '/docker-entrypoint-initdb.d/csv/group_students.csv'
    DELIMITER ',' CSV HEADER;

INSERT INTO group_students (group_id, student_id)
SELECT g.id, t.student_id
FROM tmp_group_students t
         JOIN groups g ON g.code = t.group_code
         JOIN students s ON s.student_id = t.student_id;

DROP TABLE tmp_group_students;

CREATE TEMP TABLE tmp_course_groups
(
    course_code VARCHAR(64),
    group_code  VARCHAR(64)
);

COPY tmp_course_groups (course_code, group_code)
    FROM '/docker-entrypoint-initdb.d/csv/course_groups.csv'
    DELIMITER ',' CSV HEADER;

INSERT INTO course_groups (course_id, group_code)
SELECT c.id, t.group_code
FROM tmp_course_groups t
         JOIN courses c ON c.code = t.course_code
         JOIN groups g ON g.code = t.group_code;

DROP TABLE tmp_course_groups;

CREATE TEMP TABLE tmp_semester_rooms
(
    semester_code VARCHAR(64),
    room_code     VARCHAR(64)
);

COPY tmp_semester_rooms (semester_code, room_code)
    FROM '/docker-entrypoint-initdb.d/csv/semester_rooms.csv'
    DELIMITER ',' CSV HEADER;

INSERT INTO semester_rooms (semester_id, room_code)
SELECT s.id, t.room_code
FROM tmp_semester_rooms t
         JOIN semesters s ON s.code = t.semester_code
         JOIN rooms r ON r.code = t.room_code;

DROP TABLE tmp_semester_rooms;

CREATE TEMP TABLE tmp_semester_courses
(
    semester_code VARCHAR(64),
    course_code   VARCHAR(64)
);

COPY tmp_semester_courses (semester_code, course_code)
    FROM '/docker-entrypoint-initdb.d/csv/semester_courses.csv'
    DELIMITER ',' CSV HEADER;

INSERT INTO semester_courses (semester_id, course_code)
SELECT s.id, t.course_code
FROM tmp_semester_courses t
         JOIN semesters s ON s.code = t.semester_code
         JOIN courses c ON c.code = t.course_code;

DROP TABLE tmp_semester_courses;

INSERT INTO teacher_working_hours (teacher_id_fk, day_of_week, start_time, end_time)
SELECT t.id, 'MONDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'IVANOV_AS'
UNION ALL
SELECT t.id, 'WEDNESDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'IVANOV_AS'
UNION ALL
SELECT t.id, 'FRIDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'IVANOV_AS'
UNION ALL
SELECT t.id, 'MONDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'PETROVA_MV'
UNION ALL
SELECT t.id, 'TUESDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'PETROVA_MV'
UNION ALL
SELECT t.id, 'THURSDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'PETROVA_MV'
UNION ALL
SELECT t.id, 'MONDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'SIDOROV_IP'
UNION ALL
SELECT t.id, 'TUESDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'SIDOROV_IP'
UNION ALL
SELECT t.id, 'FRIDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'SIDOROV_IP'
UNION ALL
SELECT t.id, 'TUESDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'KUZNETSOV_DA'
UNION ALL
SELECT t.id, 'WEDNESDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'KUZNETSOV_DA'
UNION ALL
SELECT t.id, 'FRIDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'KUZNETSOV_DA'
UNION ALL
SELECT t.id, 'WEDNESDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'VOLKOVA_OA'
UNION ALL
SELECT t.id, 'THURSDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'VOLKOVA_OA'
UNION ALL
SELECT t.id, 'MONDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'SMIRNOVA_EV'
UNION ALL
SELECT t.id, 'THURSDAY', '09:00'::time, '18:00'::time
FROM teachers t
WHERE t.teacher_id = 'SMIRNOVA_EV'
ON CONFLICT DO NOTHING;

INSERT INTO users (id, login, email, password_hash, role, status, created_at, updated_at)
SELECT gen_random_uuid(),
       'root',
       'root@example.com',
       '{noop}root',
       'ADMIN',
       'ACTIVE',
       now(),
       now()
WHERE NOT EXISTS (SELECT 1 FROM users WHERE login = 'root');
