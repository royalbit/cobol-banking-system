package com.royalbit.banking.api.dto;

import com.royalbit.banking.domain.AccountType;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.PositiveOrZero;

import java.math.BigDecimal;

/**
 * Request DTO for creating a new account.
 */
public record CreateAccountRequest(
        @NotBlank(message = "Account ID is required")
        @Pattern(regexp = "^[A-Za-z0-9]{1,10}$", message = "Account ID must be 1-10 alphanumeric characters")
        String accountId,

        @NotBlank(message = "Customer name is required")
        String customerName,

        @NotNull(message = "Initial balance is required")
        @PositiveOrZero(message = "Initial balance must be zero or positive")
        BigDecimal initialBalance,

        @NotNull(message = "Account type is required")
        AccountType accountType
) {}
