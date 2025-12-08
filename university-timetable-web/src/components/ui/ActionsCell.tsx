import React, {ReactNode} from 'react';
import styles from './ActionsCell.module.css';

interface ActionsCellProps {
    children: ReactNode;
}

export const ActionsCell: React.FC<ActionsCellProps> = ({children}) => (
    <td className={styles.cell}>
        <div className={styles.stack}>{children}</div>
    </td>
);
