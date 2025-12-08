export function escapeCsvField(value: string | number): string {
    const str = String(value);
    const needsQuote = /[",\n]/.test(str);
    const escaped = str.replace(/"/g, '""');
    return needsQuote ? `"${escaped}"` : escaped;
}

export function downloadCsv(
    filename: string,
    header: string[],
    rows: (string | number | null | undefined)[][],
): void {
    const headerLine = header.join(',');
    const rowLines = rows.map(row =>
        row
            .map(v => (v == null ? '' : escapeCsvField(v)))
            .join(','),
    );
    const csv = [headerLine, ...rowLines].join('\n');
    const blob = new Blob([csv], {type: 'text/csv;charset=utf-8;'});
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = filename;
    a.click();
    URL.revokeObjectURL(url);
}
