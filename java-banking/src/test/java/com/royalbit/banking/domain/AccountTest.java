package com.royalbit.banking.domain;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;

import static com.royalbit.banking.TestFixtures.*;
import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for Account domain model.
 * Validates Java implementation matches COBOL behavior.
 */
class AccountTest {

    @Nested
    @DisplayName("Account Creation")
    class AccountCreation {

        @Test
        @DisplayName("should create account with valid data")
        void createAccount() {
            Account account = savingsAccount();

            assertEquals("TEST001", account.getAccountId());
            assertEquals(30, account.getCustomerName().length()); // COBOL PIC X(30)
            assertEquals(new BigDecimal("1000.00"), account.getBalance());
            assertEquals(AccountType.SAVINGS, account.getAccountType());
            assertEquals(AccountStatus.ACTIVE, account.getStatus());
        }

        @Test
        @DisplayName("should match COBOL account ID length (10 chars)")
        void accountIdLength() {
            Account account = new Account("1234567890", padName("Test"),
                    BigDecimal.ZERO, AccountType.SAVINGS, AccountStatus.ACTIVE);
            assertEquals(10, account.getAccountId().length());
        }
    }

    @Nested
    @DisplayName("Account Type")
    class AccountTypeTests {

        @Test
        @DisplayName("should map S to SAVINGS")
        void savingsType() {
            assertEquals(AccountType.SAVINGS, AccountType.fromCode('S'));
            assertEquals('S', AccountType.SAVINGS.getCode());
        }

        @Test
        @DisplayName("should map C to CHECKING")
        void checkingType() {
            assertEquals(AccountType.CHECKING, AccountType.fromCode('C'));
            assertEquals('C', AccountType.CHECKING.getCode());
        }

        @Test
        @DisplayName("should identify savings account")
        void isSavings() {
            assertTrue(savingsAccount().isSavings());
            assertFalse(checkingAccount().isSavings());
        }
    }

    @Nested
    @DisplayName("Account Status")
    class AccountStatusTests {

        @Test
        @DisplayName("should map A to ACTIVE")
        void activeStatus() {
            assertEquals(AccountStatus.ACTIVE, AccountStatus.fromCode('A'));
            assertEquals('A', AccountStatus.ACTIVE.getCode());
        }

        @Test
        @DisplayName("should map I to INACTIVE")
        void inactiveStatus() {
            assertEquals(AccountStatus.INACTIVE, AccountStatus.fromCode('I'));
            assertEquals('I', AccountStatus.INACTIVE.getCode());
        }

        @Test
        @DisplayName("should identify active account")
        void isActive() {
            Account active = savingsAccount();
            assertTrue(active.isActive());

            active.setStatus(AccountStatus.INACTIVE);
            assertFalse(active.isActive());
        }
    }

    @Nested
    @DisplayName("COBOL Fixtures Validation")
    class CobolFixtures {

        @Test
        @DisplayName("should load all 4 accounts from CUSTOMERS.DAT")
        void loadAccounts() {
            var accounts = cobolAccounts();
            assertEquals(4, accounts.size());
        }

        @Test
        @DisplayName("should have correct account types from COBOL data")
        void accountTypes() {
            var accounts = cobolAccounts();

            // First 3 accounts are Savings, 3rd is Checking
            assertEquals(AccountType.SAVINGS, accounts.get(0).getAccountType());
            assertEquals(AccountType.SAVINGS, accounts.get(1).getAccountType());
            assertEquals(AccountType.CHECKING, accounts.get(2).getAccountType());
            assertEquals(AccountType.SAVINGS, accounts.get(3).getAccountType());
        }

        @Test
        @DisplayName("should have correct statuses from COBOL data")
        void accountStatuses() {
            var accounts = cobolAccounts();

            // John Doe (4569364kim) is Inactive, others Active
            assertEquals(AccountStatus.ACTIVE, accounts.get(0).getStatus());
            assertEquals(AccountStatus.ACTIVE, accounts.get(1).getStatus());
            assertEquals(AccountStatus.INACTIVE, accounts.get(2).getStatus());
            assertEquals(AccountStatus.ACTIVE, accounts.get(3).getStatus());
        }
    }
}
