import React from 'react';
import styles from './StatusBadge.module.css';

interface Props {
    status: string;
}

function getVariant(status: string): string {
    switch (status.toUpperCase()) {
        case 'ACTIVE': return styles.active;
        case 'ARCHIVED': return styles.archived;
        case 'INACTIVE': return styles.inactive;
        default: return styles.default;
    }
}

const LABELS: Record<string, string> = {
    ACTIVE: 'Активен',
    ARCHIVED: 'Архив',
    INACTIVE: 'Неактивен',
};

export const StatusBadge: React.FC<Props> = ({status}) => {
    const label = LABELS[status.toUpperCase()] ?? status;
    return (
        <span className={`${styles.badge} ${getVariant(status)}`}>
            {label}
        </span>
    );
};
