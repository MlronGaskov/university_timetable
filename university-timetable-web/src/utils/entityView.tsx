const HIDDEN_KEYS = ['password', 'passwordHash', 'salt'];

export const isVisibleField = (key: string) =>
    !HIDDEN_KEYS.some(hidden => key.toLowerCase().includes(hidden));
