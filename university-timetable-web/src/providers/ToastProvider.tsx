import React, {createContext, useCallback, useContext, useRef, useState} from 'react';
import styles from '@/components/ui/Toast.module.css';

type ToastType = 'success' | 'error' | 'info';

interface ToastItem {
    id: number;
    message: string;
    type: ToastType;
}

interface ToastContextValue {
    showToast: (message: string, type?: ToastType) => void;
}

const ToastContext = createContext<ToastContextValue | undefined>(undefined);

export const ToastProvider: React.FC<{children: React.ReactNode}> = ({children}) => {
    const [toasts, setToasts] = useState<ToastItem[]>([]);
    const counter = useRef(0);

    const showToast = useCallback((message: string, type: ToastType = 'info') => {
        const id = ++counter.current;
        setToasts(prev => [...prev, {id, message, type}]);
        setTimeout(() => {
            setToasts(prev => prev.filter(t => t.id !== id));
        }, 3500);
    }, []);

    return (
        <ToastContext.Provider value={{showToast}}>
            {children}
            <div className={styles.container}>
                {toasts.map(t => (
                    <div key={t.id} className={`${styles.toast} ${styles[t.type]}`}>
                        <span className={styles.icon}>
                            {t.type === 'success' ? '✓' : t.type === 'error' ? '✕' : 'ℹ'}
                        </span>
                        {t.message}
                    </div>
                ))}
            </div>
        </ToastContext.Provider>
    );
};

export const useToast = (): ToastContextValue => {
    const ctx = useContext(ToastContext);
    if (!ctx) throw new Error('useToast must be used within ToastProvider');
    return ctx;
};
