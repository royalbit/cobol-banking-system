package com.royalbit.banking.api.controller;

import com.royalbit.banking.api.dto.*;
import com.royalbit.banking.domain.Account;
import com.royalbit.banking.service.BankingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * REST API for account operations.
 * Maps to COBOL banking operations.
 */
@RestController
@RequestMapping("/api/accounts")
@Tag(name = "Accounts", description = "Account management operations")
public class AccountController {

    private final BankingService bankingService;

    public AccountController(BankingService bankingService) {
        this.bankingService = bankingService;
    }

    @GetMapping
    @Operation(summary = "Get all accounts", description = "Retrieve all accounts (COBOL: VIEW-ACCOUNTS)")
    public List<AccountResponse> getAllAccounts() {
        return bankingService.getAllAccounts().stream()
                .map(AccountResponse::from)
                .toList();
    }

    @GetMapping("/{accountId}")
    @Operation(summary = "Get account by ID", description = "Retrieve a specific account")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Account found"),
            @ApiResponse(responseCode = "404", description = "Account not found")
    })
    public ResponseEntity<AccountResponse> getAccount(@PathVariable String accountId) {
        return bankingService.findAccount(accountId)
                .map(AccountResponse::from)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @PostMapping
    @ResponseStatus(HttpStatus.CREATED)
    @Operation(summary = "Create new account", description = "Create a new bank account (COBOL: CREATE-ACCOUNT)")
    @ApiResponses({
            @ApiResponse(responseCode = "201", description = "Account created"),
            @ApiResponse(responseCode = "400", description = "Invalid request"),
            @ApiResponse(responseCode = "409", description = "Account ID already exists")
    })
    public ResponseEntity<AccountResponse> createAccount(@Valid @RequestBody CreateAccountRequest request) {
        // Check if account already exists
        if (bankingService.findAccount(request.accountId()).isPresent()) {
            return ResponseEntity.status(HttpStatus.CONFLICT).build();
        }

        Account account = bankingService.createAccount(
                request.accountId(),
                request.customerName(),
                request.initialBalance(),
                request.accountType()
        );
        return ResponseEntity.status(HttpStatus.CREATED).body(AccountResponse.from(account));
    }

    @PostMapping("/{accountId}/deposit")
    @Operation(summary = "Deposit money", description = "Deposit funds into account (COBOL: DEPOSIT-MONEY)")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Deposit successful"),
            @ApiResponse(responseCode = "400", description = "Invalid amount"),
            @ApiResponse(responseCode = "404", description = "Account not found or inactive")
    })
    public ResponseEntity<AccountResponse> deposit(
            @PathVariable String accountId,
            @Valid @RequestBody TransactionRequest request) {
        return bankingService.deposit(accountId, request.amount())
                .map(AccountResponse::from)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @PostMapping("/{accountId}/withdraw")
    @Operation(summary = "Withdraw money", description = "Withdraw funds from account (COBOL: WITHDRAW-MONEY)")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Withdrawal successful"),
            @ApiResponse(responseCode = "400", description = "Insufficient funds or invalid amount"),
            @ApiResponse(responseCode = "404", description = "Account not found or inactive")
    })
    public ResponseEntity<AccountResponse> withdraw(
            @PathVariable String accountId,
            @Valid @RequestBody TransactionRequest request) {

        BankingService.WithdrawalResult check = bankingService.checkWithdrawal(accountId, request.amount());
        return switch (check) {
            case OK -> bankingService.withdraw(accountId, request.amount())
                    .map(AccountResponse::from)
                    .map(ResponseEntity::ok)
                    .orElse(ResponseEntity.notFound().build());
            case INSUFFICIENT_FUNDS -> ResponseEntity.badRequest().build();
            case ACCOUNT_NOT_FOUND, ACCOUNT_INACTIVE -> ResponseEntity.notFound().build();
        };
    }

    @DeleteMapping("/{accountId}")
    @Operation(summary = "Delete account", description = "Soft delete an account (mark as inactive)")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Account deleted"),
            @ApiResponse(responseCode = "404", description = "Account not found")
    })
    public ResponseEntity<AccountResponse> deleteAccount(@PathVariable String accountId) {
        return bankingService.deleteAccount(accountId)
                .map(AccountResponse::from)
                .map(ResponseEntity::ok)
                .orElse(ResponseEntity.notFound().build());
    }

    @PostMapping("/apply-interest")
    @Operation(summary = "Apply interest", description = "Apply 2% interest to all savings accounts (COBOL: APPLY-INTEREST)")
    public ResponseEntity<InterestResponse> applyInterest() {
        int count = bankingService.applyInterest();
        return ResponseEntity.ok(new InterestResponse(count, "Interest applied to " + count + " savings account(s)"));
    }

    public record InterestResponse(int accountsUpdated, String message) {}
}
