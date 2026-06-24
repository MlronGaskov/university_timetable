import React from 'react';
import styles from './Input.module.css';

interface InputProps extends React.InputHTMLAttributes<HTMLInputElement> {
    hasError?: boolean;
}

export const Input = React.forwardRef<HTMLInputElement, InputProps>(
    function Input({hasError, className, ...props}, ref) {
        const cls = [styles.root, hasError ? styles.error : '', className ?? ''].filter(Boolean).join(' ');
        return <input ref={ref} className={cls} {...props} />;
    },
);
