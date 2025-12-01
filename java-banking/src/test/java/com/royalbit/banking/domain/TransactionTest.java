package com.royalbit.banking.domain;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;

import static com.royalbit.banking.TestFixtures.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for Transaction domain model.
 * Validates Java implementation matches COBOL behavior.
 */
class TransactionTest {

    @Nested
    @DisplayName("Transaction Type")
    class TransactionTypeTests {

        @Test
        @DisplayName("should map D to DEPOSIT")
        void depositType() {
            assertEquals(TransactionType.DEPOSIT, TransactionType.fromCode('D'));
            assertEquals('D', TransactionType.DEPOSIT.getCode());
        }

        @Test
        @DisplayName("should map W to WITHDRAWAL")
        void withdrawalType() {
            assertEquals(TransactionType.WITHDRAWAL, TransactionType.fromCode('W'));
            assertEquals('W', TransactionType.WITHDRAWAL.getCode());
        }

        @Test
        @DisplayName("should map I to INTEREST")
        void interestType() {
            assertEquals(TransactionType.INTEREST, TransactionType.fromCode('I'));
            assertEquals('I', TransactionType.INTEREST.getCode());
        }

        @Test
        @DisplayName("should map X to DELETE")
        void deleteType() {
            assertEquals(TransactionType.DELETE, TransactionType.fromCode('X'));
            assertEquals('X', TransactionType.DELETE.getCode());
        }
    }

    @Nested
    @DisplayName("Date/Time Formatting")
    class DateTimeFormatting {

        @Test
        @DisplayName("should format date as COBOL style YYYY/MM/DD")
        void dateFormat() {
            Transaction tx = new Transaction("TEST001", TransactionType.DEPOSIT,
                    BigDecimal.TEN, LocalDate.of(2025, 7, 27), LocalTime.of(21, 12, 16));

            assertEquals("2025/07/27", tx.getFormattedDate());
        }

        @Test
        @DisplayName("should format time as COBOL style HH:MM:SS")
        void timeFormat() {
            Transaction tx = new Transaction("TEST001", TransactionType.DEPOSIT,
                    BigDecimal.TEN, LocalDate.of(2025, 7, 27), LocalTime.of(21, 12, 16));

            assertEquals("21:12:16", tx.getFormattedTime());
        }

        @Test
        @DisplayName("should pad single-digit values with zeros")
        void paddedFormat() {
            Transaction tx = new Transaction("TEST001", TransactionType.DEPOSIT,
                    BigDecimal.TEN, LocalDate.of(2025, 1, 5), LocalTime.of(9, 5, 3));

            assertEquals("2025/01/05", tx.getFormattedDate());
            assertEquals("09:05:03", tx.getFormattedTime());
        }
    }

    @Nested
    @DisplayName("COBOL Fixtures Validation")
    class CobolFixtures {

        @Test
        @DisplayName("should load all 11 transactions from TRANSACTIONS.DAT")
        void loadTransactions() {
            var transactions = cobolTransactions();
            assertEquals(11, transactions.size());
        }

        @Test
        @DisplayName("should have correct transaction types from COBOL data")
        void transactionTypes() {
            var transactions = cobolTransactions();

            // Count by type
            long deposits = transactions.stream()
                    .filter(t -> t.getTransactionType() == TransactionType.DEPOSIT)
                    .count();
            long interest = transactions.stream()
                    .filter(t -> t.getTransactionType() == TransactionType.INTEREST)
                    .count();
            long deletes = transactions.stream()
                    .filter(t -> t.getTransactionType() == TransactionType.DELETE)
                    .count();

            assertEquals(3, deposits);
            assertEquals(7, interest);
            assertEquals(1, deletes);
        }

        @Test
        @DisplayName("should have correct account associations")
        void accountAssociations() {
            var transactions = cobolTransactions();

            // Count transactions per account
            long akeem = transactions.stream()
                    .filter(t -> t.getAccountId().equals("345akeem55"))
                    .count();
            long jemi = transactions.stream()
                    .filter(t -> t.getAccountId().equals("678jemi345"))
                    .count();
            long kim = transactions.stream()
                    .filter(t -> t.getAccountId().equals("4569364kim"))
                    .count();
            long jack = transactions.stream()
                    .filter(t -> t.getAccountId().equals("fkgreie345"))
                    .count();

            assertEquals(4, akeem);
            assertEquals(3, jemi);
            assertEquals(1, kim);
            assertEquals(3, jack);
        }
    }
}
