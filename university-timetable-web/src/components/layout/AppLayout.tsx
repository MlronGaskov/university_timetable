import React from 'react';
import {Outlet} from 'react-router-dom';
import {SidebarNav} from './SidebarNav';
import {TopBar} from './TopBar';
import styles from './AppLayout.module.css';

export const AppLayout: React.FC = () => {
    return (
        <div className={styles.root}>
            <SidebarNav/>
            <div className={styles.main}>
                <TopBar/>
                <div className={styles.content}>
                    <Outlet/>
                </div>
            </div>
        </div>
    );
};
