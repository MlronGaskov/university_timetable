package ru.nsu.university.timetable.web;

public final class IfMatchVersion {
    private IfMatchVersion() {
    }

    public static long parseRequired(String ifMatchHeader) {
        if (ifMatchHeader == null || ifMatchHeader.isBlank()) {
            throw new PreconditionRequiredException(
                    "Missing If-Match header. Read the resource, take its 'version', and retry with If-Match: <version>."
            );
        }

        String value = ifMatchHeader.trim();

        if (value.startsWith("W/")) {
            throw new PreconditionRequiredException("Weak ETag values are not supported for If-Match.");
        }

        if (value.length() >= 2 && value.startsWith("\"") && value.endsWith("\"")) {
            value = value.substring(1, value.length() - 1).trim();
        }

        if ("*".equals(value)) {
            throw new PreconditionRequiredException("If-Match: * is not supported. Send the concrete numeric version.");
        }

        try {
            long parsed = Long.parseLong(value);
            if (parsed < 0) {
                throw new NumberFormatException("negative version");
            }
            return parsed;
        } catch (NumberFormatException ex) {
            throw new PreconditionRequiredException("Invalid If-Match header. Expected a non-negative numeric version.");
        }
    }
}
