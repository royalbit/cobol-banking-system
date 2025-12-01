package com.royalbit.banking.api.controller;

import com.royalbit.banking.domain.AccountType;
import com.royalbit.banking.service.BankingService;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;

import static org.hamcrest.Matchers.*;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

/**
 * Integration tests for TransactionController.
 * Tests REST API endpoints for transaction history and mini-statements.
 */
@SpringBootTest
@AutoConfigureMockMvc
class TransactionControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private BankingService bankingService;

    @BeforeEach
    void setUp() {
        // Create test account with transactions
        if (bankingService.findAccount("TXN001").isEmpty()) {
            bankingService.createAccount("TXN001", "Transaction Test", new BigDecimal("1000.00"), AccountType.SAVINGS);
            bankingService.deposit("TXN001", new BigDecimal("100.00"));
            bankingService.deposit("TXN001", new BigDecimal("200.00"));
            bankingService.withdraw("TXN001", new BigDecimal("50.00"));
        }
    }

    @Nested
    @DisplayName("GET /api/accounts/{accountId}/transactions")
    class GetTransactionHistory {

        @Test
        @DisplayName("should return transaction history")
        void getHistorySuccess() throws Exception {
            mockMvc.perform(get("/api/accounts/TXN001/transactions"))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(jsonPath("$", hasSize(greaterThanOrEqualTo(3))))
                    .andExpect(jsonPath("$[0].accountId", is("TXN001")))
                    .andExpect(jsonPath("$[0].type", notNullValue()))
                    .andExpect(jsonPath("$[0].typeLabel", notNullValue()))
                    .andExpect(jsonPath("$[0].amount", notNullValue()))
                    .andExpect(jsonPath("$[0].date", matchesPattern("\\d{4}/\\d{2}/\\d{2}")))
                    .andExpect(jsonPath("$[0].time", matchesPattern("\\d{2}:\\d{2}:\\d{2}")));
        }

        @Test
        @DisplayName("should return 404 for non-existent account")
        void getHistoryNotFound() throws Exception {
            mockMvc.perform(get("/api/accounts/NOTEXIST/transactions"))
                    .andExpect(status().isNotFound());
        }

        @Test
        @DisplayName("should return empty list for account with no transactions")
        void getHistoryEmpty() throws Exception {
            // Create account without transactions
            if (bankingService.findAccount("TXN002").isEmpty()) {
                bankingService.createAccount("TXN002", "No Transactions", new BigDecimal("500.00"), AccountType.CHECKING);
            }

            mockMvc.perform(get("/api/accounts/TXN002/transactions"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$", hasSize(0)));
        }
    }

    @Nested
    @DisplayName("GET /api/accounts/{accountId}/transactions/mini-statement")
    class GetMiniStatement {

        @Test
        @DisplayName("should return mini statement (last 5 transactions)")
        void getMiniStatementSuccess() throws Exception {
            // Create account with many transactions
            if (bankingService.findAccount("TXN003").isEmpty()) {
                bankingService.createAccount("TXN003", "Mini Statement Test", new BigDecimal("5000.00"), AccountType.SAVINGS);
                for (int i = 0; i < 7; i++) {
                    bankingService.deposit("TXN003", new BigDecimal("100.00"));
                }
            }

            mockMvc.perform(get("/api/accounts/TXN003/transactions/mini-statement"))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(jsonPath("$", hasSize(lessThanOrEqualTo(5))));
        }

        @Test
        @DisplayName("should return 404 for non-existent account")
        void getMiniStatementNotFound() throws Exception {
            mockMvc.perform(get("/api/accounts/NOTEXIST/transactions/mini-statement"))
                    .andExpect(status().isNotFound());
        }

        @Test
        @DisplayName("should include COBOL-style display labels")
        void getMiniStatementLabels() throws Exception {
            mockMvc.perform(get("/api/accounts/TXN001/transactions/mini-statement"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$[*].typeLabel", everyItem(matchesPattern("DEP|WTH|INT|DEL"))));
        }
    }
}
