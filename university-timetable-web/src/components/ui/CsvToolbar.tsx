import React from 'react';
import {Button} from '@/components/ui/Button';
import styles from './CsvToolbar.module.css';

interface CsvToolbarProps {
    onExport: () => void;
    onImportFile: (file: File) => void;
    importDisabled?: boolean;
    showCreate?: boolean;
    createLabel?: string;
    onCreate?: () => void;
}

export const CsvToolbar: React.FC<CsvToolbarProps> = ({
                                                          onExport,
                                                          onImportFile,
                                                          importDisabled,
                                                          showCreate = false,
                                                          createLabel = 'Создать',
                                                          onCreate,
                                                      }) => {
    const handleChange: React.ChangeEventHandler<HTMLInputElement> = e => {
        const file = e.target.files?.[0];
        e.target.value = '';
        if (file) onImportFile(file);
    };

    return (
        <div className={styles.root}>
            <Button variant="secondary" onClick={onExport}>
                Экспорт CSV
            </Button>
            <label className={styles.importLabel}>
                <span className={styles.importText}>Импорт CSV</span>
                <input
                    type="file"
                    accept=".csv"
                    className={styles.importInput}
                    onChange={handleChange}
                    disabled={importDisabled}
                />
            </label>
            {showCreate && onCreate && (
                <Button onClick={onCreate}>{createLabel}</Button>
            )}
        </div>
    );
};
