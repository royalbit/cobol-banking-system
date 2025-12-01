package com.royalbit.banking;

import com.royalbit.banking.domain.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;

/**
 * Test fixtures derived from COBOL .DAT files.
 *
 * Source: CUSTOMERS.DAT, TRANSACTIONS.DAT
 * These fixtures represent the exact state of the COBOL system
 * for equivalence testing.
 */
public class TestFixtures {

    /**
     * Accounts from CUSTOMERS.DAT:
     * 345akeem55Akeem Mohammed                     2441SA
     * 678jemi345Jemi Mohammed                      1592SA
     * 4569364kimJohn Doe                          10000CI
     * fkgreie345Jack Loom                         57720SA
     */
    public static List<Account> cobolAccounts() {
        return List.of(
            new Account("345akeem55", padName("Akeem Mohammed"),
                        new BigDecimal("24.41"), AccountType.SAVINGS, AccountStatus.ACTIVE),
            new Account("678jemi345", padName("Jemi Mohammed"),
                        new BigDecimal("15.92"), AccountType.SAVINGS, AccountStatus.ACTIVE),
            new Account("4569364kim", padName("John Doe"),
                        new BigDecimal("100.00"), AccountType.CHECKING, AccountStatus.INACTIVE),
            new Account("fkgreie345", padName("Jack Loom"),
                        new BigDecimal("577.20"), AccountType.SAVINGS, AccountStatus.ACTIVE)
        );
    }

    /**
     * Transactions from TRANSACTIONS.DAT:
     * 345akeem55D     20002025/07/2721:12:16
     * 345akeem55I       462025/07/2721:12:55
     * 678jemi345I       302025/07/2721:12:55
     * 4569364kimX        02025/07/2721:21:02
     * 345akeem55I       472025/07/2722:25:28
     * 678jemi345I       312025/07/2722:25:28
     * 345akeem55I       482025/07/2722:26:19
     * 678jemi345I       312025/07/2722:26:19
     * fkgreie345I     11202025/07/2722:26:19
     * fkgreie345D      5002025/07/2717:46:54
     * fkgreie345D      1002025/07/2717:47:58
     */
    public static List<Transaction> cobolTransactions() {
        return List.of(
            tx("345akeem55", TransactionType.DEPOSIT, "200.00", "2025-07-27", "21:12:16"),
            tx("345akeem55", TransactionType.INTEREST, "4.6", "2025-07-27", "21:12:55"),
            tx("678jemi345", TransactionType.INTEREST, "3.0", "2025-07-27", "21:12:55"),
            tx("4569364kim", TransactionType.DELETE, "0", "2025-07-27", "21:21:02"),
            tx("345akeem55", TransactionType.INTEREST, "4.7", "2025-07-27", "22:25:28"),
            tx("678jemi345", TransactionType.INTEREST, "3.1", "2025-07-27", "22:25:28"),
            tx("345akeem55", TransactionType.INTEREST, "4.8", "2025-07-27", "22:26:19"),
            tx("678jemi345", TransactionType.INTEREST, "3.1", "2025-07-27", "22:26:19"),
            tx("fkgreie345", TransactionType.INTEREST, "112.0", "2025-07-27", "22:26:19"),
            tx("fkgreie345", TransactionType.DEPOSIT, "50.0", "2025-07-27", "17:46:54"),
            tx("fkgreie345", TransactionType.DEPOSIT, "10.0", "2025-07-27", "17:47:58")
        );
    }

    /**
     * Sample account for unit tests.
     */
    public static Account savingsAccount() {
        return new Account("TEST001", padName("Test User"),
                new BigDecimal("1000.00"), AccountType.SAVINGS, AccountStatus.ACTIVE);
    }

    /**
     * Sample checking account for unit tests.
     */
    public static Account checkingAccount() {
        return new Account("TEST002", padName("Test Checking"),
                new BigDecimal("500.00"), AccountType.CHECKING, AccountStatus.ACTIVE);
    }

    /**
     * Pad name to 30 characters (COBOL PIC X(30)).
     */
    public static String padName(String name) {
        return String.format("%-30s", name);
    }

    private static Transaction tx(String accountId, TransactionType type,
                                   String amount, String date, String time) {
        return new Transaction(
                accountId,
                type,
                new BigDecimal(amount),
                LocalDate.parse(date),
                LocalTime.parse(time)
        );
    }
}
