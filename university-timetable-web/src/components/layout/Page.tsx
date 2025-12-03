import React, {ReactNode} from 'react';
import styles from './Page.module.css';

interface PageProps {
    title: string;
    actions?: ReactNode;
    children: ReactNode;
}

export const Page: React.FC<PageProps> = ({title, actions, children}) => {
    return (
        <div className={styles.root}>
            <div className={styles.header}>
                <h1 className={styles.title}>{title}</h1>
                {actions && <div className={styles.actions}>{actions}</div>}
            </div>
            <div className={styles.body}>{children}</div>
        </div>
    );
};
