package com.royalbit.banking.service;

import com.royalbit.banking.domain.*;
import com.royalbit.banking.repository.AccountRepository;
import com.royalbit.banking.repository.TransactionRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.orm.jpa.DataJpaTest;
import org.springframework.context.annotation.Import;

import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for BankingService verifying COBOL equivalence.
 *
 * Each test documents the equivalent COBOL behavior being verified.
 */
@DataJpaTest
@Import(BankingService.class)
@DisplayName("BankingService - COBOL Equivalence Tests")
class BankingServiceTest {

    @Autowired
    private BankingService bankingService;

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private TransactionRepository transactionRepository;

    @Nested
    @DisplayName("CREATE-ACCOUNT (COBOL Option 1)")
    class CreateAccount {

        @Test
        @DisplayName("should create account with padded name like COBOL")
        void createAccountPadsName() {
            // COBOL: MOVE WS-NAME TO NAME (PIC X(30) pads with spaces)
            Account account = bankingService.createAccount(
                    "TEST00001", "John Doe", new BigDecimal("100.00"), AccountType.SAVINGS);

            assertEquals(30, account.getCustomerName().length());
            assertTrue(account.getCustomerName().startsWith("John Doe"));
        }

        @Test
        @DisplayName("should create account with ACTIVE status by default")
        void createAccountActive() {
            // COBOL: New accounts default to 'A' status
            Account account = bankingService.createAccount(
                    "TEST00002", "Jane Doe", new BigDecimal("500.00"), AccountType.CHECKING);

            assertEquals(AccountStatus.ACTIVE, account.getStatus());
            assertTrue(account.isActive());
        }

        @Test
        @DisplayName("should persist account to repository")
        void createAccountPersists() {
            // COBOL: WRITE CUSTOMER-RECORD
            bankingService.createAccount(
                    "TEST00003", "Bob Smith", new BigDecimal("250.00"), AccountType.SAVINGS);

            Optional<Account> found = accountRepository.findById("TEST00003");
            assertTrue(found.isPresent());
            assertEquals("Bob Smith", found.get().getCustomerName().trim());
        }
    }

    @Nested
    @DisplayName("VIEW-ACCOUNTS (COBOL Option 2)")
    class ViewAccounts {

        @BeforeEach
        void setupAccounts() {
            bankingService.createAccount("VIEW001", "Alice", new BigDecimal("100.00"), AccountType.SAVINGS);
            bankingService.createAccount("VIEW002", "Bob", new BigDecimal("200.00"), AccountType.CHECKING);
            bankingService.createAccount("VIEW003", "Charlie", new BigDecimal("300.00"), AccountType.SAVINGS);
        }

        @Test
        @DisplayName("should return all accounts like COBOL VIEW-ACCOUNTS")
        void viewAllAccounts() {
            // COBOL: PERFORM UNTIL FILE-STATUS = "10" READ CUSTOMER-FILE
            List<Account> accounts = bankingService.getAllAccounts();
            assertEquals(3, accounts.size());
        }

        @Test
        @DisplayName("should find account by ID like COBOL search")
        void findAccountById() {
            // COBOL: IF ACCT-ID = WS-SEARCH-ID
            Optional<Account> found = bankingService.findAccount("VIEW002");

            assertTrue(found.isPresent());
            assertEquals("Bob", found.get().getCustomerName().trim());
        }

        @Test
        @DisplayName("should return empty for non-existent account")
        void findAccountNotFound() {
            // COBOL: WS-FOUND = 'N' when account not in file
            Optional<Account> found = bankingService.findAccount("NOTEXIST");
            assertTrue(found.isEmpty());
        }
    }

    @Nested
    @DisplayName("DEPOSIT-MONEY (COBOL Option 3)")
    class DepositMoney {

        @BeforeEach
        void setupAccount() {
            bankingService.createAccount("DEP001", "Depositor", new BigDecimal("100.00"), AccountType.SAVINGS);
        }

        @Test
        @DisplayName("should add amount to balance like COBOL")
        void depositAddsToBalance() {
            // COBOL: ADD WS-AMOUNT TO BALANCE
            Optional<Account> result = bankingService.deposit("DEP001", new BigDecimal("50.00"));

            assertTrue(result.isPresent());
            assertEquals(0, new BigDecimal("150.00").compareTo(result.get().getBalance()));
        }

        @Test
        @DisplayName("should log deposit transaction")
        void depositLogsTransaction() {
            // COBOL: PERFORM LOG-TRANSACTION-DEPOSIT
            bankingService.deposit("DEP001", new BigDecimal("75.00"));

            List<Transaction> transactions = transactionRepository.findByAccountId("DEP001");
            assertEquals(1, transactions.size());
            assertEquals(TransactionType.DEPOSIT, transactions.get(0).getTransactionType());
            assertEquals(0, new BigDecimal("75.00").compareTo(transactions.get(0).getAmount()));
        }

        @Test
        @DisplayName("should return empty for non-existent account")
        void depositAccountNotFound() {
            // COBOL: DISPLAY "Account not found: " WS-SEARCH-ID
            Optional<Account> result = bankingService.deposit("NOTEXIST", new BigDecimal("100.00"));
            assertTrue(result.isEmpty());
        }

        @Test
        @DisplayName("should not deposit to inactive account")
        void depositToInactiveAccount() {
            // COBOL: Inactive accounts cannot receive deposits
            bankingService.deleteAccount("DEP001"); // Mark inactive

            Optional<Account> result = bankingService.deposit("DEP001", new BigDecimal("50.00"));
            assertTrue(result.isEmpty());
        }
    }

    @Nested
    @DisplayName("WITHDRAW-MONEY (COBOL Option 4)")
    class WithdrawMoney {

        @BeforeEach
        void setupAccount() {
            bankingService.createAccount("WTH001", "Withdrawer", new BigDecimal("100.00"), AccountType.CHECKING);
        }

        @Test
        @DisplayName("should subtract amount from balance like COBOL")
        void withdrawSubtractsFromBalance() {
            // COBOL: SUBTRACT WS-AMOUNT FROM BALANCE
            Optional<Account> result = bankingService.withdraw("WTH001", new BigDecimal("30.00"));

            assertTrue(result.isPresent());
            assertEquals(0, new BigDecimal("70.00").compareTo(result.get().getBalance()));
        }

        @Test
        @DisplayName("should log withdrawal transaction")
        void withdrawLogsTransaction() {
            // COBOL: PERFORM LOG-TRANSACTION-WITHDRAW
            bankingService.withdraw("WTH001", new BigDecimal("25.00"));

            List<Transaction> transactions = transactionRepository.findByAccountId("WTH001");
            assertEquals(1, transactions.size());
            assertEquals(TransactionType.WITHDRAWAL, transactions.get(0).getTransactionType());
        }

        @Test
        @DisplayName("should prevent overdraft like COBOL")
        void withdrawPreventOverdraft() {
            // COBOL: IF BALANCE >= WS-AMOUNT ... ELSE DISPLAY "Insufficient funds!"
            Optional<Account> result = bankingService.withdraw("WTH001", new BigDecimal("150.00"));

            assertTrue(result.isEmpty());

            // Balance should be unchanged
            Account account = accountRepository.findById("WTH001").orElseThrow();
            assertEquals(0, new BigDecimal("100.00").compareTo(account.getBalance()));
        }

        @Test
        @DisplayName("should allow withdrawal of exact balance")
        void withdrawExactBalance() {
            // COBOL: IF BALANCE >= WS-AMOUNT (equals case)
            Optional<Account> result = bankingService.withdraw("WTH001", new BigDecimal("100.00"));

            assertTrue(result.isPresent());
            assertEquals(0, BigDecimal.ZERO.compareTo(result.get().getBalance()));
        }

        @Test
        @DisplayName("should report insufficient funds correctly")
        void checkWithdrawalInsufficientFunds() {
            // COBOL: DISPLAY "Insufficient funds!" + DISPLAY "Current balance: $" BALANCE
            BankingService.WithdrawalResult result =
                    bankingService.checkWithdrawal("WTH001", new BigDecimal("500.00"));

            assertEquals(BankingService.WithdrawalResult.INSUFFICIENT_FUNDS, result);
        }

        @Test
        @DisplayName("should report account not found")
        void checkWithdrawalNotFound() {
            BankingService.WithdrawalResult result =
                    bankingService.checkWithdrawal("NOTEXIST", new BigDecimal("10.00"));

            assertEquals(BankingService.WithdrawalResult.ACCOUNT_NOT_FOUND, result);
        }
    }

    @Nested
    @DisplayName("DELETE Account (COBOL Option 7 - soft delete)")
    class DeleteAccount {

        @BeforeEach
        void setupAccount() {
            bankingService.createAccount("DEL001", "ToDelete", new BigDecimal("50.00"), AccountType.SAVINGS);
        }

        @Test
        @DisplayName("should mark account as INACTIVE")
        void deleteMarksInactive() {
            // COBOL: Sets status to 'I'
            Optional<Account> result = bankingService.deleteAccount("DEL001");

            assertTrue(result.isPresent());
            assertEquals(AccountStatus.INACTIVE, result.get().getStatus());
            assertFalse(result.get().isActive());
        }

        @Test
        @DisplayName("should log DELETE transaction")
        void deleteLogsTransaction() {
            // COBOL: Logs 'X' transaction type
            bankingService.deleteAccount("DEL001");

            List<Transaction> transactions = transactionRepository.findByAccountId("DEL001");
            assertTrue(transactions.stream()
                    .anyMatch(t -> t.getTransactionType() == TransactionType.DELETE));
        }

        @Test
        @DisplayName("deleted account should reject deposits")
        void deletedAccountRejectsDeposit() {
            bankingService.deleteAccount("DEL001");

            Optional<Account> result = bankingService.deposit("DEL001", new BigDecimal("100.00"));
            assertTrue(result.isEmpty());
        }

        @Test
        @DisplayName("deleted account should reject withdrawals")
        void deletedAccountRejectsWithdrawal() {
            bankingService.deleteAccount("DEL001");

            Optional<Account> result = bankingService.withdraw("DEL001", new BigDecimal("10.00"));
            assertTrue(result.isEmpty());
        }
    }

    @Nested
    @DisplayName("APPLY-INTEREST (COBOL Option 6)")
    class ApplyInterest {

        @BeforeEach
        void setupAccounts() {
            // Mix of savings and checking accounts
            bankingService.createAccount("INT001", "Saver1", new BigDecimal("1000.00"), AccountType.SAVINGS);
            bankingService.createAccount("INT002", "Saver2", new BigDecimal("500.00"), AccountType.SAVINGS);
            bankingService.createAccount("INT003", "Checker", new BigDecimal("2000.00"), AccountType.CHECKING);
        }

        @Test
        @DisplayName("should calculate 2% interest like COBOL")
        void calculateInterest() {
            // COBOL: COMPUTE WS-AMOUNT = BALANCE * 0.02
            bankingService.applyInterest();

            Account saver1 = accountRepository.findById("INT001").orElseThrow();
            // 1000 * 0.02 = 20, new balance = 1020
            assertEquals(0, new BigDecimal("1020.00").compareTo(saver1.getBalance()));
        }

        @Test
        @DisplayName("should only apply interest to SAVINGS accounts")
        void interestOnlySavings() {
            // COBOL: IF ACCT-TYPE = 'S'
            bankingService.applyInterest();

            Account checker = accountRepository.findById("INT003").orElseThrow();
            // Checking account should be unchanged
            assertEquals(0, new BigDecimal("2000.00").compareTo(checker.getBalance()));
        }

        @Test
        @DisplayName("should return count of accounts processed")
        void interestReturnsCount() {
            // COBOL: ADD 1 TO WS-STMT-COUNT
            int count = bankingService.applyInterest();
            assertEquals(2, count); // Only 2 savings accounts
        }

        @Test
        @DisplayName("should log INTEREST transaction for each account")
        void interestLogsTransactions() {
            // COBOL: PERFORM LOG-TRANSACTION-INTEREST
            bankingService.applyInterest();

            List<Transaction> int1Transactions = transactionRepository.findByAccountId("INT001");
            assertTrue(int1Transactions.stream()
                    .anyMatch(t -> t.getTransactionType() == TransactionType.INTEREST));

            List<Transaction> int3Transactions = transactionRepository.findByAccountId("INT003");
            assertTrue(int3Transactions.stream()
                    .noneMatch(t -> t.getTransactionType() == TransactionType.INTEREST));
        }

        @Test
        @DisplayName("should not apply interest to inactive savings accounts")
        void interestSkipsInactive() {
            bankingService.deleteAccount("INT001"); // Mark inactive

            int count = bankingService.applyInterest();
            assertEquals(1, count); // Only INT002 should get interest

            Account inactive = accountRepository.findById("INT001").orElseThrow();
            assertEquals(0, new BigDecimal("1000.00").compareTo(inactive.getBalance())); // Unchanged
        }
    }

    @Nested
    @DisplayName("Transaction Logging")
    class TransactionLogging {

        @Test
        @DisplayName("should record date in COBOL format YYYY/MM/DD")
        void transactionDateFormat() {
            bankingService.createAccount("LOG001", "Logger", new BigDecimal("100.00"), AccountType.SAVINGS);
            bankingService.deposit("LOG001", new BigDecimal("50.00"));

            Transaction tx = transactionRepository.findByAccountId("LOG001").get(0);
            String formattedDate = tx.getFormattedDate();

            // COBOL format: YYYY/MM/DD
            assertTrue(formattedDate.matches("\\d{4}/\\d{2}/\\d{2}"));
        }

        @Test
        @DisplayName("should record time in COBOL format HH:MM:SS")
        void transactionTimeFormat() {
            bankingService.createAccount("LOG002", "Timer", new BigDecimal("100.00"), AccountType.SAVINGS);
            bankingService.deposit("LOG002", new BigDecimal("25.00"));

            Transaction tx = transactionRepository.findByAccountId("LOG002").get(0);
            String formattedTime = tx.getFormattedTime();

            // COBOL format: HH:MM:SS
            assertTrue(formattedTime.matches("\\d{2}:\\d{2}:\\d{2}"));
        }
    }
}
