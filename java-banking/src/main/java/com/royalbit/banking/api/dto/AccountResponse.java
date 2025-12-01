package com.royalbit.banking.api.dto;

import com.royalbit.banking.domain.Account;
import com.royalbit.banking.domain.AccountStatus;
import com.royalbit.banking.domain.AccountType;

import java.math.BigDecimal;

/**
 * Response DTO for account information.
 */
public record AccountResponse(
        String accountId,
        String customerName,
        BigDecimal balance,
        AccountType accountType,
        AccountStatus status
) {
    public static AccountResponse from(Account account) {
        return new AccountResponse(
                account.getAccountId(),
                account.getCustomerName().trim(),
                account.getBalance(),
                account.getAccountType(),
                account.getStatus()
        );
    }
}
