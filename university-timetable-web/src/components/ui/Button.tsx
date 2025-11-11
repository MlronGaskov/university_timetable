import React from 'react';

export default function Button(props: React.ButtonHTMLAttributes<HTMLButtonElement>) {
    const {className = '', type, ...rest} = props;
    return <button type={type ?? 'button'} {...rest} className={`btn ${className}`}/>;
}