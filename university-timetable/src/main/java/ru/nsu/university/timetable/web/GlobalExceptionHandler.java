package ru.nsu.university.timetable.web;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.ConstraintViolationException;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.*;

@RestControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(MethodArgumentNotValidException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public String handleValidation(MethodArgumentNotValidException ex) {
        var f = ex.getBindingResult().getFieldErrors();
        if (!f.isEmpty()) return f.get(0).getDefaultMessage();
        return "Validation error";
    }

    @ExceptionHandler(ConstraintViolationException.class)
    @ResponseStatus(HttpStatus.BAD_REQUEST)
    public String handleConstraint(ConstraintViolationException ex) {
        return ex.getConstraintViolations().stream().findFirst()
                .map(ConstraintViolation::getMessage).orElse("Validation error");
    }
}
