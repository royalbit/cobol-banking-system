package com.royalbit.banking.domain;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalTime;
import java.util.Objects;

/**
 * Transaction entity matching COBOL TRANSACTION-RECORD.
 *
 * COBOL layout:
 *   05 TRANS-ACCT-ID    PIC X(10)    -> accountId
 *   05 TRANS-TYPE       PIC X(1)     -> transactionType (D/W/I/X)
 *   05 TRANS-AMOUNT     PIC 9(7)V99  -> amount (max 9999999.99)
 *   05 TRANS-DATE       PIC X(10)    -> date (YYYY/MM/DD)
 *   05 TRANS-TIME       PIC X(8)     -> time (HH:MM:SS)
 */
@Entity
@Table(name = "transactions")
public class Transaction {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "account_id", length = 10, nullable = false)
    private String accountId;

    @Enumerated(EnumType.STRING)
    @Column(name = "transaction_type", nullable = false)
    private TransactionType transactionType;

    @Column(name = "amount", precision = 9, scale = 2, nullable = false)
    private BigDecimal amount;

    @Column(name = "transaction_date", nullable = false)
    private LocalDate date;

    @Column(name = "transaction_time", nullable = false)
    private LocalTime time;

    protected Transaction() {
    }

    public Transaction(String accountId, TransactionType transactionType,
                       BigDecimal amount, LocalDate date, LocalTime time) {
        this.accountId = accountId;
        this.transactionType = transactionType;
        this.amount = amount;
        this.date = date;
        this.time = time;
    }

    public Long getId() {
        return id;
    }

    public String getAccountId() {
        return accountId;
    }

    public TransactionType getTransactionType() {
        return transactionType;
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public LocalDate getDate() {
        return date;
    }

    public LocalTime getTime() {
        return time;
    }

    /**
     * Format date as COBOL style: YYYY/MM/DD
     */
    public String getFormattedDate() {
        return String.format("%04d/%02d/%02d",
                date.getYear(), date.getMonthValue(), date.getDayOfMonth());
    }

    /**
     * Format time as COBOL style: HH:MM:SS
     */
    public String getFormattedTime() {
        return String.format("%02d:%02d:%02d",
                time.getHour(), time.getMinute(), time.getSecond());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Transaction that = (Transaction) o;
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }

    @Override
    public String toString() {
        return String.format("Transaction[%s, %s, $%.2f, %s %s]",
                accountId, transactionType, amount, getFormattedDate(), getFormattedTime());
    }
}
