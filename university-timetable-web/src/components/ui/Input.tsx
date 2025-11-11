import React from 'react';

type Props = React.InputHTMLAttributes<HTMLInputElement>;

const Input = React.forwardRef<HTMLInputElement, Props>(function InputBase({className = '', ...rest}, ref) {
    return <input ref={ref} {...rest} className={`input ${className}`}/>;
});

export default Input;