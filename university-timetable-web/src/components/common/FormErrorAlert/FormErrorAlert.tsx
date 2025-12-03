import type {FC} from 'react';
import './FormErrorAlert.css';
import type {ApiErrorResponse} from '@/types/api-error';

interface Props {
    apiError?: ApiErrorResponse | null;
    fallbackMessage?: string | null;
}

export const FormErrorAlert: FC<Props> = ({apiError, fallbackMessage}) => {
    if (!apiError && !fallbackMessage) return null;

    const message = apiError?.message || fallbackMessage || '';

    return (
        <div className="FormErrorAlert">
            {message && <div className="FormErrorAlert-main">{message}</div>}
            {apiError && apiError.fieldErrors.length > 0 && (
                <ul className="FormErrorAlert-list">
                    {apiError.fieldErrors.map((f, idx) => (
                        <li key={idx}>
                            {f.field ? <strong>{f.field}: </strong> : null}
                            {f.message}
                        </li>
                    ))}
                </ul>
            )}
        </div>
    );
};
