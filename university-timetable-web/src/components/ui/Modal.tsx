import React from 'react';

export default function Modal({title, children, onClose}: {
    title: string;
    children: React.ReactNode;
    onClose: () => void
}) {
    return (
        <div className="modal-backdrop">
            <div className="modal">
                <div className="modal-hdr">
                    <h2 style={{fontWeight: 600}}>{title}</h2>
                    <button className="btn" onClick={onClose}>×</button>
                </div>
                <div className="modal-body">{children}</div>
            </div>
        </div>
    );
}
