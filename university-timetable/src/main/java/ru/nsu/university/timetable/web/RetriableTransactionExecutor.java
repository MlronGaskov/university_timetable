package ru.nsu.university.timetable.web;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.CannotAcquireLockException;
import org.springframework.dao.ConcurrencyFailureException;
import org.springframework.dao.DeadlockLoserDataAccessException;
import org.springframework.dao.PessimisticLockingFailureException;
import org.springframework.orm.ObjectOptimisticLockingFailureException;
import org.springframework.dao.OptimisticLockingFailureException;
import org.springframework.stereotype.Component;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.function.Supplier;

@Component
@RequiredArgsConstructor
@Slf4j
public class RetriableTransactionExecutor {
    private static final int MAX_ATTEMPTS = 3;

    private final PlatformTransactionManager transactionManager;

    public void execute(String operationName, Runnable action) {
        execute(operationName, () -> {
            action.run();
            return null;
        });
    }

    public <T> T execute(String operationName, Supplier<T> action) {
        RuntimeException lastFailure = null;

        for (int attempt = 1; attempt <= MAX_ATTEMPTS; attempt++) {
            try {
                return new TransactionTemplate(transactionManager).execute(status -> action.get());
            } catch (RuntimeException ex) {
                if (!isRetryable(ex) || attempt == MAX_ATTEMPTS) {
                    throw ex;
                }

                lastFailure = ex;
                log.warn("Retry transactional operation '{}' after attempt {}", operationName, attempt, ex);
            }
        }

        throw lastFailure;
    }

    private boolean isRetryable(RuntimeException ex) {
        return ex instanceof CannotAcquireLockException
                || ex instanceof ConcurrencyFailureException
                || ex instanceof DeadlockLoserDataAccessException
                || ex instanceof ObjectOptimisticLockingFailureException
                || ex instanceof OptimisticLockingFailureException
                || ex instanceof PessimisticLockingFailureException;
    }
}
