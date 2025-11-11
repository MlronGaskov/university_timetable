UPDATE users
SET password_hash = '{bcrypt}' || crypt('root', gen_salt('bf', 10))
WHERE login = 'root';

SELECT login, crypt('root', substring(password_hash from 10)) = substring(password_hash from 10) AS ok
FROM users
WHERE login = 'root';
