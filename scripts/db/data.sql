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

INSERT INTO teachers (teacher_id, full_name, status, created_at, updated_at)
VALUES
    ('IVANOV_AS',    'Иванов Алексей Сергеевич',      'ACTIVE', now(), now()),
    ('PETROVA_MV',   'Петрова Мария Викторовна',      'ACTIVE', now(), now()),
    ('SIDOROV_IP',   'Сидоров Илья Петрович',         'ACTIVE', now(), now()),
    ('KUZNETSOV_DA', 'Кузнецов Дмитрий Андреевич',    'ACTIVE', now(), now()),
    ('VOLKOVA_OA',   'Волкова Ольга Алексеевна',      'ACTIVE', now(), now()),
    ('SMIRNOVA_EV',  'Смирнова Евгения Викторовна',   'ACTIVE', now(), now())
ON CONFLICT (teacher_id) DO NOTHING;

INSERT INTO groups (name, code, size, status, created_at, updated_at)
VALUES
    ('CS 25-01', '25201', 26, 'ACTIVE', now(), now()),
    ('CS 24-01', '24201', 26, 'ACTIVE', now(), now()),
    ('CS 24-02', '24202', 28, 'ACTIVE', now(), now()),
    ('CS 23-01', '23201', 24, 'ACTIVE', now(), now()),
    ('CS 23-02', '23202', 25, 'ACTIVE', now(), now()),
    ('CS 22-01', '22201', 22, 'ACTIVE', now(), now()),
    ('CS 22-02', '22202', 21, 'ACTIVE', now(), now())
ON CONFLICT (code) DO NOTHING;

INSERT INTO rooms (code, building, number, capacity, status, created_at, updated_at)
VALUES
    ('402 ГК',  'ГК',       '402', 120, 'ACTIVE', now(), now()),

    ('311 КПА', 'КПА',      '311',  60, 'ACTIVE', now(), now()),

    ('1125',    'Корпус 1', '1125', 40, 'ACTIVE', now(), now()),
    ('2128',    'Корпус 2', '2128', 40, 'ACTIVE', now(), now())
ON CONFLICT (code) DO NOTHING;

-- (заглушка, чтобы семестр ссылался на неё)

INSERT INTO policies (name, grid_json, breaks_json, limits_json, travel_matrix_json, weights_json, created_at, updated_at)
VALUES (
    'default-policy',
    '{}',
    '[]',
    '{}',
    '{}',
    '{}',
    now(),
    now()
)
ON CONFLICT (name) DO NOTHING;

INSERT INTO courses (code, title, teacher_id, planned_hours, status, created_at, updated_at)
VALUES
    ('ALG',   'Алгоритмы и структуры данных',         'IVANOV_AS',    4, 'ACTIVE', now(), now()),
    ('ALG-P', 'Практикум по алгоритмам',              'SMIRNOVA_EV',  2, 'ACTIVE', now(), now()),
    ('LA',    'Линейная алгебра',                     'PETROVA_MV',   4, 'ACTIVE', now(), now()),
    ('LA-P',  'Семинар по линейной алгебре',          'PETROVA_MV',   2, 'ACTIVE', now(), now()),
    ('PROG-C','Программирование на C',                'SIDOROV_IP',   4, 'ACTIVE', now(), now()),
    ('OS',    'Операционные системы',                 'KUZNETSOV_DA', 4, 'ACTIVE', now(), now()),
    ('OS-L',  'Лабораторный практикум по ОС',         'KUZNETSOV_DA', 2, 'ACTIVE', now(), now()),
    ('ML',    'Введение в машинное обучение',         'VOLKOVA_OA',   4, 'ACTIVE', now(), now()),
    ('ML-P',  'Практикум по машинному обучению',      'VOLKOVA_OA',   2, 'ACTIVE', now(), now())
ON CONFLICT (code) DO NOTHING;

INSERT INTO course_groups (course_id, group_code)
SELECT c.id, g.code
FROM courses c
JOIN groups g ON g.code IN ('25201')
WHERE c.code IN ('ALG', 'ALG-P', 'LA', 'LA-P', 'PROG-C')
ON CONFLICT DO NOTHING;

INSERT INTO course_groups (course_id, group_code)
SELECT c.id, g.code
FROM courses c
JOIN groups g ON g.code IN ('24201', '24202')
WHERE c.code IN ('OS', 'OS-L')
ON CONFLICT DO NOTHING;

INSERT INTO course_groups (course_id, group_code)
SELECT c.id, g.code
FROM courses c
JOIN groups g ON g.code IN ('23201', '23202')
WHERE c.code IN ('ML', 'ML-P')
ON CONFLICT DO NOTHING;

INSERT INTO semesters (code, start_at, end_at, policy_name, status, created_at, updated_at)
VALUES (
    '2025-fall',
    '2025-09-01T00:00:00Z',
    '2025-12-31T23:59:59Z',
    'default-policy',
    'ACTIVE',
    now(),
    now()
)
ON CONFLICT (code) DO NOTHING;

INSERT INTO semester_courses (semester_id, course_code)
SELECT s.id, c.code
FROM semesters s
CROSS JOIN courses c
WHERE s.code = '2025-fall'
  AND c.code IN ('ALG', 'ALG-P', 'LA', 'LA-P', 'PROG-C', 'OS', 'OS-L', 'ML', 'ML-P')
ON CONFLICT DO NOTHING;

INSERT INTO semester_rooms (semester_id, room_code)
SELECT s.id, r.code
FROM semesters s
CROSS JOIN rooms r
WHERE s.code = '2025-fall'
  AND r.code IN ('402 ГК', '311 КПА', '1125', '2128')
ON CONFLICT DO NOTHING;

INSERT INTO schedules (id, semester_code, version, evaluation_score, created_at, updated_at)
VALUES (
    '22222222-2222-2222-2222-222222222222',
    '2025-fall',
    1,
    0.95,
    now(),
    now()
)
ON CONFLICT (semester_code, version) DO NOTHING;

INSERT INTO schedule_slots (
    id,
    schedule_id,
    course_code,
    room_code,
    day_of_week,
    start_time,
    end_time,
    valid_from,
    valid_until,
    week_pattern,
    created_at,
    updated_at
) VALUES
    ('33333333-3333-3333-3333-333333330001',
     '22222222-2222-2222-2222-222222222222',
     'ALG',
     '402 ГК',
     'MONDAY',
     '09:00:00', '10:35:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330002',
     '22222222-2222-2222-2222-222222222222',
     'LA',
     '311 КПА',
     'MONDAY',
     '10:50:00', '12:25:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330003',
     '22222222-2222-2222-2222-222222222222',
     'PROG-C',
     '2128',
     'MONDAY',
     '12:40:00', '14:15:00',
     '2025-09-01', '2025-12-31',
     'ODD_WEEKS',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330004',
     '22222222-2222-2222-2222-222222222222',
     'ALG-P',
     '1125',
     'MONDAY',
     '14:30:00', '16:05:00',
     '2025-09-01', '2025-12-31',
     'EVEN_WEEKS',
     now(), now()
    ),

    ('33333333-3333-3333-3333-333333330005',
     '22222222-2222-2222-2222-222222222222',
     'OS',
     '402 ГК',
     'TUESDAY',
     '09:00:00', '10:35:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330006',
     '22222222-2222-2222-2222-222222222222',
     'OS-L',
     '311 КПА',
     'TUESDAY',
     '10:50:00', '12:25:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330007',
     '22222222-2222-2222-2222-222222222222',
     'LA-P',
     '1125',
     'TUESDAY',
     '12:40:00', '14:15:00',
     '2025-09-01', '2025-12-31',
     'ODD_WEEKS',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330008',
     '22222222-2222-2222-2222-222222222222',
     'PROG-C',
     '2128',
     'TUESDAY',
     '14:30:00', '16:05:00',
     '2025-09-01', '2025-12-31',
     'EVEN_WEEKS',
     now(), now()
    ),

    ('33333333-3333-3333-3333-333333330009',
     '22222222-2222-2222-2222-222222222222',
     'ML',
     '311 КПА',
     'WEDNESDAY',
     '09:00:00', '10:35:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330010',
     '22222222-2222-2222-2222-222222222222',
     'ML-P',
     '2128',
     'WEDNESDAY',
     '10:50:00', '12:25:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330011',
     '22222222-2222-2222-2222-222222222222',
     'ALG',
     '402 ГК',
     'WEDNESDAY',
     '12:40:00', '14:15:00',
     '2025-09-01', '2025-12-31',
     'ODD_WEEKS',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330012',
     '22222222-2222-2222-2222-222222222222',
     'OS',
     '1125',
     'WEDNESDAY',
     '14:30:00', '16:05:00',
     '2025-09-01', '2025-12-31',
     'EVEN_WEEKS',
     now(), now()
    ),

    ('33333333-3333-3333-3333-333333330013',
     '22222222-2222-2222-2222-222222222222',
     'LA',
     '402 ГК',
     'THURSDAY',
     '09:00:00', '10:35:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330014',
     '22222222-2222-2222-2222-222222222222',
     'ALG-P',
     '2128',
     'THURSDAY',
     '10:50:00', '12:25:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330015',
     '22222222-2222-2222-2222-222222222222',
     'ML',
     '311 КПА',
     'THURSDAY',
     '12:40:00', '14:15:00',
     '2025-09-01', '2025-12-31',
     'ODD_WEEKS',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330016',
     '22222222-2222-2222-2222-222222222222',
     'ML-P',
     '2128',
     'THURSDAY',
     '14:30:00', '16:05:00',
     '2025-09-01', '2025-12-31',
     'EVEN_WEEKS',
     now(), now()
    ),

    ('33333333-3333-3333-3333-333333330017',
     '22222222-2222-2222-2222-222222222222',
     'PROG-C',
     '1125',
     'FRIDAY',
     '09:00:00', '10:35:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330018',
     '22222222-2222-2222-2222-222222222222',
     'OS-L',
     '311 КПА',
     'FRIDAY',
     '10:50:00', '12:25:00',
     '2025-09-01', '2025-12-31',
     'EVERY_WEEK',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330019',
     '22222222-2222-2222-2222-222222222222',
     'ALG',
     '402 ГК',
     'FRIDAY',
     '12:40:00', '14:15:00',
     '2025-09-01', '2025-12-31',
     'ODD_WEEKS',
     now(), now()
    ),
    ('33333333-3333-3333-3333-333333330020',
     '22222222-2222-2222-2222-222222222222',
     'LA',
     '402 ГК',
     'FRIDAY',
     '14:30:00', '16:05:00',
     '2025-09-01', '2025-12-31',
     'EVEN_WEEKS',
     now(), now()
    )
ON CONFLICT (id) DO NOTHING;
