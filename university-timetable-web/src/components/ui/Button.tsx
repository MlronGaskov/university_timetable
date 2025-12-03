import React from 'react';
import styles from './Button.module.css';

interface ButtonProps extends React.ButtonHTMLAttributes<HTMLButtonElement> {
    variant?: 'primary' | 'secondary' | 'ghost';
}

export const Button: React.FC<ButtonProps> = ({variant = 'primary', className, ...rest}) => {
    const cls = [styles.root, styles[variant], className].filter(Boolean).join(' ');
    return <button className={cls} {...rest} />;
};
