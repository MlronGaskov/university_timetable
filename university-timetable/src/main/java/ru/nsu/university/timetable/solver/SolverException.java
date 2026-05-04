package ru.nsu.university.timetable.solver;

import lombok.Getter;
import ru.nsu.university.timetable.solver.dto.SolverErrorResponse;

@Getter
public class SolverException extends RuntimeException {
    private final Integer httpStatus;
    private final SolverErrorResponse errorResponse;

    public SolverException(String message, Integer httpStatus, SolverErrorResponse errorResponse) {
        super(message);
        this.httpStatus = httpStatus;
        this.errorResponse = errorResponse;
    }

    public SolverException(String message, Integer httpStatus, SolverErrorResponse errorResponse, Throwable cause) {
        super(message, cause);
        this.httpStatus = httpStatus;
        this.errorResponse = errorResponse;
    }

}
