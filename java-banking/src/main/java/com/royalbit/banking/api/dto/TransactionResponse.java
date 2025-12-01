package com.royalbit.banking.api.dto;

import com.royalbit.banking.domain.Transaction;
import com.royalbit.banking.domain.TransactionType;

import java.math.BigDecimal;

/**
 * Response DTO for transaction information.
 */
public record TransactionResponse(
        Long id,
        String accountId,
        TransactionType type,
        String typeLabel,
        BigDecimal amount,
        String date,
        String time
) {
    public static TransactionResponse from(Transaction tx) {
        return new TransactionResponse(
                tx.getId(),
                tx.getAccountId(),
                tx.getTransactionType(),
                tx.getTransactionType().getDisplayLabel(),
                tx.getAmount(),
                tx.getFormattedDate(),
                tx.getFormattedTime()
        );
    }
}
