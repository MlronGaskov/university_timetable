package ru.nsu.university.timetable.web;

import org.junit.jupiter.api.Test;
import org.springframework.dao.CannotAcquireLockException;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.TransactionStatus;
import org.springframework.transaction.support.SimpleTransactionStatus;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

class RetriableTransactionExecutorTest {

    private final RetriableTransactionExecutor executor =
            new RetriableTransactionExecutor(new NoOpTransactionManager());

    @Test
    void retriesWholeTransactionOnTransientLockFailure() {
        AtomicInteger attempts = new AtomicInteger();

        String result = executor.execute("test.retry", () -> {
            if (attempts.incrementAndGet() < 2) {
                throw new CannotAcquireLockException("retry me");
            }
            return "ok";
        });

        assertEquals("ok", result);
        assertEquals(2, attempts.get());
    }

    @Test
    void doesNotRetryNonTransientFailures() {
        AtomicInteger attempts = new AtomicInteger();

        assertThrows(
                ResourceConflictException.class,
                () -> executor.execute("test.no-retry", () -> {
                    attempts.incrementAndGet();
                    throw new ResourceConflictException("stale");
                })
        );

        assertEquals(1, attempts.get());
    }

    private static final class NoOpTransactionManager implements PlatformTransactionManager {
        @Override
        public TransactionStatus getTransaction(TransactionDefinition definition) {
            return new SimpleTransactionStatus();
        }

        @Override
        public void commit(TransactionStatus status) {
        }

        @Override
        public void rollback(TransactionStatus status) {
        }
    }
}
