import React, {ReactNode} from 'react';
import styles from './FormField.module.css';

interface FormFieldProps {
    label: string;
    children: ReactNode;
    error?: string | null;
}

export const FormField: React.FC<FormFieldProps> = ({label, children, error}) => {
    return (
        <div className={styles.root}>
            <label className={styles.label}>{label}</label>
            {children}
            {error && <div className={styles.error}>{error}</div>}
        </div>
    );
};
