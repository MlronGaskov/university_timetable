import React from 'react';

type Props = React.SelectHTMLAttributes<HTMLSelectElement>;

const Select = React.forwardRef<HTMLSelectElement, Props>(function SelectBase({ className = '', children, ...rest }, ref) {
    return (
        <select ref={ref} {...rest} className={`select ${className}`}>
            {children}
        </select>
    );
});

export default Select;