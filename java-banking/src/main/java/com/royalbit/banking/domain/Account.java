package com.royalbit.banking.domain;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.util.Objects;

/**
 * Account entity matching COBOL CUSTOMER-RECORD.
 *
 * COBOL layout:
 *   05 ACCT-ID     PIC X(10)    -> accountId
 *   05 NAME        PIC X(30)    -> customerName
 *   05 BALANCE     PIC 9(7)V99  -> balance (max 9999999.99)
 *   05 ACCT-TYPE   PIC X(1)     -> accountType (S/C)
 *   + STATUS field              -> status (A/I)
 */
@Entity
@Table(name = "accounts")
public class Account {

    @Id
    @Column(name = "account_id", length = 10)
    private String accountId;

    @Column(name = "customer_name", length = 30, nullable = false)
    private String customerName;

    @Column(name = "balance", precision = 9, scale = 2, nullable = false)
    private BigDecimal balance;

    @Enumerated(EnumType.STRING)
    @Column(name = "account_type", nullable = false)
    private AccountType accountType;

    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    private AccountStatus status;

    protected Account() {
    }

    public Account(String accountId, String customerName, BigDecimal balance,
                   AccountType accountType, AccountStatus status) {
        this.accountId = accountId;
        this.customerName = customerName;
        this.balance = balance;
        this.accountType = accountType;
        this.status = status;
    }

    public String getAccountId() {
        return accountId;
    }

    public String getCustomerName() {
        return customerName;
    }

    public BigDecimal getBalance() {
        return balance;
    }

    public AccountType getAccountType() {
        return accountType;
    }

    public AccountStatus getStatus() {
        return status;
    }

    public void setBalance(BigDecimal balance) {
        this.balance = balance;
    }

    public void setStatus(AccountStatus status) {
        this.status = status;
    }

    public boolean isActive() {
        return status == AccountStatus.ACTIVE;
    }

    public boolean isSavings() {
        return accountType == AccountType.SAVINGS;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Account account = (Account) o;
        return Objects.equals(accountId, account.accountId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(accountId);
    }

    @Override
    public String toString() {
        return String.format("Account[%s, %s, $%.2f, %s, %s]",
                accountId, customerName.trim(), balance, accountType, status);
    }
}
