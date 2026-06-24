import React from 'react';
import styles from './EmptyState.module.css';

interface Props {
    icon?: React.ReactNode;
    title: string;
    description?: string;
}

export const EmptyState: React.FC<Props> = ({icon, title, description}) => (
    <div className={styles.root}>
        {icon && <div className={styles.icon}>{icon}</div>}
        <div className={styles.title}>{title}</div>
        {description && <div className={styles.description}>{description}</div>}
    </div>
);
