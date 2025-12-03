import React from 'react';
import {Outlet} from 'react-router-dom';
import styles from './PublicLayout.module.css';

export const PublicLayout: React.FC = () => {
    return (
        <div className={styles.root}>
            <div className={styles.card}>
                <Outlet/>
            </div>
        </div>
    );
};
