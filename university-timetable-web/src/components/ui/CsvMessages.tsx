import React from 'react';
import styles from './CsvMessages.module.css';

interface CsvMessagesProps {
    error: string | null;
    result: string | null;
}

export const CsvMessages: React.FC<CsvMessagesProps> = ({error, result}) => {
    if (!error && !result) return null;
    return (
        <div className={styles.root}>
            {error && <p className={styles.error}>{error}</p>}
            {result && <p className={styles.result}>{result}</p>}
        </div>
    );
};
