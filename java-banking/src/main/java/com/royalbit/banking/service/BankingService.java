package com.royalbit.banking.service;

import com.royalbit.banking.domain.*;
import com.royalbit.banking.repository.AccountRepository;
import com.royalbit.banking.repository.TransactionRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.List;
import java.util.Optional;

/**
 * Core banking operations matching COBOL PROCEDURE DIVISION.
 *
 * COBOL equivalents:
 * - CREATE-ACCOUNT → createAccount()
 * - VIEW-ACCOUNTS → getAllAccounts()
 * - DEPOSIT-MONEY → deposit()
 * - WITHDRAW-MONEY → withdraw()
 * - APPLY-INTEREST → applyInterest()
 */
@Service
@Transactional
public class BankingService {

    private final AccountRepository accountRepository;
    private final TransactionRepository transactionRepository;

    public BankingService(AccountRepository accountRepository,
                          TransactionRepository transactionRepository) {
        this.accountRepository = accountRepository;
        this.transactionRepository = transactionRepository;
    }

    /**
     * Create new account (COBOL: CREATE-ACCOUNT).
     *
     * COBOL: WRITE CUSTOMER-RECORD
     */
    public Account createAccount(String accountId, String customerName,
                                  BigDecimal initialBalance, AccountType accountType) {
        String paddedName = padName(customerName);
        Account account = new Account(accountId, paddedName, initialBalance,
                accountType, AccountStatus.ACTIVE);
        return accountRepository.save(account);
    }

    /**
     * View all accounts (COBOL: VIEW-ACCOUNTS).
     *
     * COBOL: OPEN INPUT CUSTOMER-FILE, READ CUSTOMER-FILE UNTIL EOF
     */
    public List<Account> getAllAccounts() {
        return accountRepository.findAll();
    }

    /**
     * Find account by ID.
     *
     * COBOL: IF ACCT-ID = WS-SEARCH-ID
     */
    public Optional<Account> findAccount(String accountId) {
        return accountRepository.findById(accountId);
    }

    /**
     * Deposit money (COBOL: DEPOSIT-MONEY + UPDATE-BALANCE-ADD).
     *
     * COBOL:
     *   ADD WS-AMOUNT TO BALANCE
     *   REWRITE CUSTOMER-RECORD
     *   PERFORM LOG-TRANSACTION-DEPOSIT
     *
     * @return Updated account, or empty if account not found
     */
    public Optional<Account> deposit(String accountId, BigDecimal amount) {
        return accountRepository.findById(accountId)
                .filter(Account::isActive)
                .map(account -> {
                    BigDecimal newBalance = account.getBalance().add(amount);
                    account.setBalance(newBalance);
                    accountRepository.save(account);
                    logTransaction(accountId, TransactionType.DEPOSIT, amount);
                    return account;
                });
    }

    /**
     * Withdraw money (COBOL: WITHDRAW-MONEY + UPDATE-BALANCE-SUBTRACT).
     *
     * COBOL:
     *   IF BALANCE >= WS-AMOUNT
     *     SUBTRACT WS-AMOUNT FROM BALANCE
     *     REWRITE CUSTOMER-RECORD
     *     PERFORM LOG-TRANSACTION-WITHDRAW
     *   ELSE
     *     DISPLAY "Insufficient funds!"
     *
     * @return Updated account, or empty if account not found or insufficient funds
     */
    public Optional<Account> withdraw(String accountId, BigDecimal amount) {
        return accountRepository.findById(accountId)
                .filter(Account::isActive)
                .filter(account -> account.getBalance().compareTo(amount) >= 0)
                .map(account -> {
                    BigDecimal newBalance = account.getBalance().subtract(amount);
                    account.setBalance(newBalance);
                    accountRepository.save(account);
                    logTransaction(accountId, TransactionType.WITHDRAWAL, amount);
                    return account;
                });
    }

    /**
     * Check if withdrawal would succeed (for error reporting).
     */
    public WithdrawalResult checkWithdrawal(String accountId, BigDecimal amount) {
        Optional<Account> accountOpt = accountRepository.findById(accountId);

        if (accountOpt.isEmpty()) {
            return WithdrawalResult.ACCOUNT_NOT_FOUND;
        }

        Account account = accountOpt.get();

        if (!account.isActive()) {
            return WithdrawalResult.ACCOUNT_INACTIVE;
        }

        if (account.getBalance().compareTo(amount) < 0) {
            return WithdrawalResult.INSUFFICIENT_FUNDS;
        }

        return WithdrawalResult.OK;
    }

    /**
     * Soft delete account (COBOL: mark as inactive).
     *
     * COBOL: Sets status to 'I', logs DELETE transaction
     */
    public Optional<Account> deleteAccount(String accountId) {
        return accountRepository.findById(accountId)
                .map(account -> {
                    account.setStatus(AccountStatus.INACTIVE);
                    accountRepository.save(account);
                    logTransaction(accountId, TransactionType.DELETE, BigDecimal.ZERO);
                    return account;
                });
    }

    /**
     * Apply interest to all savings accounts (COBOL: APPLY-INTEREST).
     *
     * COBOL:
     *   IF ACCT-TYPE = 'S'
     *     COMPUTE WS-AMOUNT = BALANCE * 0.02
     *     ADD WS-AMOUNT TO BALANCE
     *     REWRITE CUSTOMER-RECORD
     *     PERFORM LOG-TRANSACTION-INTEREST
     *
     * @return Number of accounts that received interest
     */
    public int applyInterest() {
        List<Account> savingsAccounts = accountRepository
                .findByAccountTypeAndStatus(AccountType.SAVINGS, AccountStatus.ACTIVE);

        int count = 0;
        for (Account account : savingsAccounts) {
            BigDecimal interest = account.getBalance()
                    .multiply(new BigDecimal("0.02"));
            BigDecimal newBalance = account.getBalance().add(interest);
            account.setBalance(newBalance);
            accountRepository.save(account);
            logTransaction(account.getAccountId(), TransactionType.INTEREST, interest);
            count++;
        }

        return count;
    }

    /**
     * Log transaction (COBOL: LOG-TRANSACTION-*).
     *
     * COBOL:
     *   PERFORM GET-CURRENT-DATETIME
     *   OPEN EXTEND TRANSACTION-FILE
     *   WRITE TRANSACTION-RECORD
     */
    private void logTransaction(String accountId, TransactionType type, BigDecimal amount) {
        Transaction transaction = new Transaction(
                accountId,
                type,
                amount,
                LocalDate.now(),
                LocalTime.now()
        );
        transactionRepository.save(transaction);
    }

    /**
     * Pad customer name to 30 characters (COBOL: PIC X(30)).
     */
    private String padName(String name) {
        return String.format("%-30s", name);
    }

    /**
     * Result codes for withdrawal validation.
     */
    public enum WithdrawalResult {
        OK,
        ACCOUNT_NOT_FOUND,
        ACCOUNT_INACTIVE,
        INSUFFICIENT_FUNDS
    }
}
