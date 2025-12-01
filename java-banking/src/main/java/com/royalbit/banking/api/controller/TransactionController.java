package com.royalbit.banking.api.controller;

import com.royalbit.banking.api.dto.TransactionResponse;
import com.royalbit.banking.service.BankingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * REST API for transaction operations.
 * Maps to COBOL transaction operations.
 */
@RestController
@RequestMapping("/api/accounts/{accountId}/transactions")
@Tag(name = "Transactions", description = "Transaction history and statements")
public class TransactionController {

    private final BankingService bankingService;

    public TransactionController(BankingService bankingService) {
        this.bankingService = bankingService;
    }

    @GetMapping
    @Operation(summary = "Get transaction history", description = "Get full transaction history for an account")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Transaction history returned"),
            @ApiResponse(responseCode = "404", description = "Account not found")
    })
    public ResponseEntity<List<TransactionResponse>> getTransactionHistory(@PathVariable String accountId) {
        // Check if account exists
        if (bankingService.findAccount(accountId).isEmpty()) {
            return ResponseEntity.notFound().build();
        }

        List<TransactionResponse> transactions = bankingService.getTransactionHistory(accountId).stream()
                .map(TransactionResponse::from)
                .toList();
        return ResponseEntity.ok(transactions);
    }

    @GetMapping("/mini-statement")
    @Operation(summary = "Get mini statement", description = "Get last 5 transactions (COBOL: MINI-STATEMENT)")
    @ApiResponses({
            @ApiResponse(responseCode = "200", description = "Mini statement returned"),
            @ApiResponse(responseCode = "404", description = "Account not found")
    })
    public ResponseEntity<List<TransactionResponse>> getMiniStatement(@PathVariable String accountId) {
        // Check if account exists
        if (bankingService.findAccount(accountId).isEmpty()) {
            return ResponseEntity.notFound().build();
        }

        List<TransactionResponse> transactions = bankingService.getMiniStatement(accountId).stream()
                .map(TransactionResponse::from)
                .toList();
        return ResponseEntity.ok(transactions);
    }
}
