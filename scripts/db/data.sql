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