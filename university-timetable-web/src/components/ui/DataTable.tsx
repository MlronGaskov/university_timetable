import React, {ReactNode} from 'react';
import styles from './DataTable.module.css';

interface DataTableProps {
    children: ReactNode;
}

export const DataTable: React.FC<DataTableProps> = ({children}) => {
    return (
        <div className={styles.wrapper}>
            <table className={styles.table}>
                {children}
            </table>
        </div>
    );
};
