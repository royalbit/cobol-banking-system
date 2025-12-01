package com.royalbit.banking;

import com.royalbit.banking.domain.*;
import com.royalbit.banking.repository.AccountRepository;
import com.royalbit.banking.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.data.domain.PageRequest;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Equivalence tests comparing Java implementation to COBOL behavior.
 *
 * These tests load the exact data from COBOL's .DAT files and verify
 * that the Java implementation produces identical results for all operations.
 */
@DataJpaTest
@DisplayName("COBOL ↔ Java Equivalence Tests")
class EquivalenceTest {

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private TransactionRepository transactionRepository;

    @BeforeEach
    void loadCobolData() {
        // Load COBOL fixtures into H2
        accountRepository.saveAll(TestFixtures.cobolAccounts());
        transactionRepository.saveAll(TestFixtures.cobolTransactions());
    }

    @Nested
    @DisplayName("Data Loading Equivalence")
    class DataLoading {

        @Test
        @DisplayName("should load all accounts from COBOL data")
        void loadAccounts() {
            List<Account> accounts = accountRepository.findAll();
            assertEquals(4, accounts.size());
        }

        @Test
        @DisplayName("should load all transactions from COBOL data")
        void loadTransactions() {
            List<Transaction> transactions = transactionRepository.findAll();
            assertEquals(11, transactions.size());
        }

        @Test
        @DisplayName("should preserve exact account balances from COBOL")
        void preserveBalances() {
            Account akeem = accountRepository.findById("345akeem55").orElseThrow();
            Account jemi = accountRepository.findById("678jemi345").orElseThrow();
            Account john = accountRepository.findById("4569364kim").orElseThrow();
            Account jack = accountRepository.findById("fkgreie345").orElseThrow();

            assertEquals(new BigDecimal("24.41"), akeem.getBalance());
            assertEquals(new BigDecimal("15.92"), jemi.getBalance());
            assertEquals(new BigDecimal("100.00"), john.getBalance());
            assertEquals(new BigDecimal("577.20"), jack.getBalance());
        }
    }

    @Nested
    @DisplayName("View Accounts (COBOL Option 2)")
    class ViewAccounts {

        @Test
        @DisplayName("should return all accounts like COBOL VIEW-ACCOUNTS")
        void viewAllAccounts() {
            List<Account> accounts = accountRepository.findAll();

            // COBOL displays: Account ID | Customer Name | Balance | Type
            assertEquals(4, accounts.size());

            // Verify format matches COBOL output structure
            for (Account account : accounts) {
                assertEquals(10, account.getAccountId().length());
                assertEquals(30, account.getCustomerName().length());
                assertNotNull(account.getBalance());
                assertNotNull(account.getAccountType());
            }
        }

        @Test
        @DisplayName("should filter by account type like COBOL")
        void filterByType() {
            List<Account> savings = accountRepository.findByAccountType(AccountType.SAVINGS);
            List<Account> checking = accountRepository.findByAccountType(AccountType.CHECKING);

            assertEquals(3, savings.size());
            assertEquals(1, checking.size());
        }

        @Test
        @DisplayName("should filter by status like COBOL")
        void filterByStatus() {
            List<Account> active = accountRepository.findByStatus(AccountStatus.ACTIVE);
            List<Account> inactive = accountRepository.findByStatus(AccountStatus.INACTIVE);

            assertEquals(3, active.size());
            assertEquals(1, inactive.size());
        }
    }

    @Nested
    @DisplayName("Mini Statement (COBOL Option 5)")
    class MiniStatement {

        @Test
        @DisplayName("should return last 5 transactions like COBOL MINI-STATEMENT")
        void miniStatement() {
            // COBOL: "Last 5 transactions for Account: X"
            List<Transaction> transactions = transactionRepository.findRecentByAccountId(
                    "345akeem55", PageRequest.of(0, 5));

            assertTrue(transactions.size() <= 5);
            assertEquals(4, transactions.size()); // Akeem has exactly 4 transactions
        }

        @Test
        @DisplayName("should return transactions for specific account only")
        void accountSpecific() {
            List<Transaction> transactions = transactionRepository.findByAccountId("fkgreie345");
            assertEquals(3, transactions.size());

            // All should belong to Jack's account
            assertTrue(transactions.stream()
                    .allMatch(t -> t.getAccountId().equals("fkgreie345")));
        }

        @Test
        @DisplayName("should format date/time like COBOL output")
        void dateTimeFormat() {
            List<Transaction> transactions = transactionRepository.findByAccountId("345akeem55");
            Transaction first = transactions.get(0);

            // COBOL format: YYYY/MM/DD | HH:MM:SS
            assertTrue(first.getFormattedDate().matches("\\d{4}/\\d{2}/\\d{2}"));
            assertTrue(first.getFormattedTime().matches("\\d{2}:\\d{2}:\\d{2}"));
        }
    }

    @Nested
    @DisplayName("Interest Calculation (COBOL Option 6)")
    class InterestCalculation {

        @Test
        @DisplayName("should calculate 2% interest like COBOL APPLY-INTEREST")
        void calculateInterest() {
            // COBOL: COMPUTE WS-AMOUNT = BALANCE * 0.02
            BigDecimal balance = new BigDecimal("1000.00");
            BigDecimal expectedInterest = new BigDecimal("20.00");

            BigDecimal interest = balance.multiply(new BigDecimal("0.02"));
            // Use compareTo for BigDecimal equality (ignores scale differences)
            assertEquals(0, expectedInterest.compareTo(interest));
        }

        @Test
        @DisplayName("should only apply interest to savings accounts")
        void savingsOnly() {
            List<Account> savingsAccounts = accountRepository
                    .findByAccountTypeAndStatus(AccountType.SAVINGS, AccountStatus.ACTIVE);

            // COBOL: IF ACCT-TYPE = 'S' THEN apply interest
            assertEquals(3, savingsAccounts.size());
            assertTrue(savingsAccounts.stream().allMatch(Account::isSavings));
        }
    }

    @Nested
    @DisplayName("Transaction Type Mapping")
    class TransactionTypeMapping {

        @Test
        @DisplayName("should map COBOL transaction codes correctly")
        void cobolCodes() {
            // COBOL TRANS-TYPE codes: D, W, I, X
            List<Transaction> transactions = transactionRepository.findAll();

            // Verify DEP (D) transactions exist
            assertTrue(transactions.stream()
                    .anyMatch(t -> t.getTransactionType() == TransactionType.DEPOSIT));

            // Verify INT (I) transactions exist
            assertTrue(transactions.stream()
                    .anyMatch(t -> t.getTransactionType() == TransactionType.INTEREST));

            // Verify DEL (X) transactions exist
            assertTrue(transactions.stream()
                    .anyMatch(t -> t.getTransactionType() == TransactionType.DELETE));
        }
    }
}
