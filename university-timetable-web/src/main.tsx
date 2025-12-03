import React from 'react';
import ReactDOM from 'react-dom/client';
import {BrowserRouter} from 'react-router-dom';
import {AuthProvider} from '@/providers/AuthProvider';
import {AppRouter} from '@/routes/AppRouter';
import '@/styles/variables.css';
import '@/styles/globals.css';

const rootEl = document.getElementById('root') as HTMLElement;

ReactDOM.createRoot(rootEl).render(
    <React.StrictMode>
        <BrowserRouter>
            <AuthProvider>
                <AppRouter/>
            </AuthProvider>
        </BrowserRouter>
    </React.StrictMode>,
);
