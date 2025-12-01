package com.royalbit.banking.api.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.royalbit.banking.api.dto.CreateAccountRequest;
import com.royalbit.banking.api.dto.TransactionRequest;
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
 * Integration tests for AccountController.
 * Tests REST API endpoints with full Spring context.
 */
@SpringBootTest
@AutoConfigureMockMvc
@TestMethodOrder(MethodOrderer.OrderAnnotation.class)
class AccountControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private BankingService bankingService;

    @BeforeEach
    void setUp() {
        // Create test account if not exists
        if (bankingService.findAccount("TEST001").isEmpty()) {
            bankingService.createAccount("TEST001", "Test User", new BigDecimal("1000.00"), AccountType.SAVINGS);
        }
    }

    @Nested
    @DisplayName("GET /api/accounts")
    class GetAllAccounts {

        @Test
        @DisplayName("should return list of accounts")
        void getAllAccounts() throws Exception {
            mockMvc.perform(get("/api/accounts"))
                    .andExpect(status().isOk())
                    .andExpect(content().contentType(MediaType.APPLICATION_JSON))
                    .andExpect(jsonPath("$", hasSize(greaterThanOrEqualTo(1))))
                    .andExpect(jsonPath("$[0].accountId", notNullValue()));
        }
    }

    @Nested
    @DisplayName("GET /api/accounts/{accountId}")
    class GetAccount {

        @Test
        @DisplayName("should return account when exists")
        void getExistingAccount() throws Exception {
            mockMvc.perform(get("/api/accounts/TEST001"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.accountId", is("TEST001")))
                    .andExpect(jsonPath("$.customerName", is("Test User")))
                    .andExpect(jsonPath("$.accountType", is("SAVINGS")));
        }

        @Test
        @DisplayName("should return 404 when account not found")
        void getNotFoundAccount() throws Exception {
            mockMvc.perform(get("/api/accounts/NOTEXIST"))
                    .andExpect(status().isNotFound());
        }
    }

    @Nested
    @DisplayName("POST /api/accounts")
    class CreateAccount {

        @Test
        @DisplayName("should create new account")
        void createAccount() throws Exception {
            CreateAccountRequest request = new CreateAccountRequest(
                    "NEW001",
                    "New Customer",
                    new BigDecimal("500.00"),
                    AccountType.CHECKING
            );

            mockMvc.perform(post("/api/accounts")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.accountId", is("NEW001")))
                    .andExpect(jsonPath("$.customerName", is("New Customer")))
                    .andExpect(jsonPath("$.balance", is(500.00)))
                    .andExpect(jsonPath("$.accountType", is("CHECKING")));
        }

        @Test
        @DisplayName("should return 409 when account already exists")
        void createDuplicateAccount() throws Exception {
            CreateAccountRequest request = new CreateAccountRequest(
                    "TEST001",
                    "Duplicate",
                    new BigDecimal("100.00"),
                    AccountType.SAVINGS
            );

            mockMvc.perform(post("/api/accounts")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isConflict());
        }

        @Test
        @DisplayName("should return 400 for invalid request")
        void createInvalidAccount() throws Exception {
            String invalidJson = """
                    {
                        "accountId": "",
                        "customerName": "",
                        "initialBalance": -100,
                        "accountType": "SAVINGS"
                    }
                    """;

            mockMvc.perform(post("/api/accounts")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(invalidJson))
                    .andExpect(status().isBadRequest());
        }
    }

    @Nested
    @DisplayName("POST /api/accounts/{accountId}/deposit")
    class Deposit {

        @Test
        @DisplayName("should deposit money successfully")
        void depositSuccess() throws Exception {
            // Create fresh account for deposit test
            bankingService.createAccount("DEP001", "Deposit Test", new BigDecimal("100.00"), AccountType.SAVINGS);

            TransactionRequest request = new TransactionRequest(new BigDecimal("50.00"));

            mockMvc.perform(post("/api/accounts/DEP001/deposit")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.balance", is(150.00)));
        }

        @Test
        @DisplayName("should return 404 for non-existent account")
        void depositNotFound() throws Exception {
            TransactionRequest request = new TransactionRequest(new BigDecimal("50.00"));

            mockMvc.perform(post("/api/accounts/NOTEXIST/deposit")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isNotFound());
        }

        @Test
        @DisplayName("should return 400 for invalid amount")
        void depositInvalidAmount() throws Exception {
            String invalidJson = """
                    {
                        "amount": -50.00
                    }
                    """;

            mockMvc.perform(post("/api/accounts/TEST001/deposit")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(invalidJson))
                    .andExpect(status().isBadRequest());
        }
    }

    @Nested
    @DisplayName("POST /api/accounts/{accountId}/withdraw")
    class Withdraw {

        @Test
        @DisplayName("should withdraw money successfully")
        void withdrawSuccess() throws Exception {
            // Create fresh account for withdraw test
            bankingService.createAccount("WTH001", "Withdraw Test", new BigDecimal("200.00"), AccountType.CHECKING);

            TransactionRequest request = new TransactionRequest(new BigDecimal("50.00"));

            mockMvc.perform(post("/api/accounts/WTH001/withdraw")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.balance", is(150.00)));
        }

        @Test
        @DisplayName("should return 400 for insufficient funds")
        void withdrawInsufficientFunds() throws Exception {
            // Create account with low balance
            bankingService.createAccount("WTH002", "Low Balance", new BigDecimal("10.00"), AccountType.CHECKING);

            TransactionRequest request = new TransactionRequest(new BigDecimal("500.00"));

            mockMvc.perform(post("/api/accounts/WTH002/withdraw")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isBadRequest());
        }

        @Test
        @DisplayName("should return 404 for non-existent account")
        void withdrawNotFound() throws Exception {
            TransactionRequest request = new TransactionRequest(new BigDecimal("50.00"));

            mockMvc.perform(post("/api/accounts/NOTEXIST/withdraw")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isNotFound());
        }
    }

    @Nested
    @DisplayName("DELETE /api/accounts/{accountId}")
    class DeleteAccount {

        @Test
        @DisplayName("should soft delete account")
        void deleteSuccess() throws Exception {
            // Create account to delete
            bankingService.createAccount("DEL001", "To Delete", new BigDecimal("100.00"), AccountType.SAVINGS);

            mockMvc.perform(delete("/api/accounts/DEL001"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.status", is("INACTIVE")));
        }

        @Test
        @DisplayName("should return 404 for non-existent account")
        void deleteNotFound() throws Exception {
            mockMvc.perform(delete("/api/accounts/NOTEXIST"))
                    .andExpect(status().isNotFound());
        }
    }

    @Nested
    @DisplayName("POST /api/accounts/apply-interest")
    class ApplyInterest {

        @Test
        @DisplayName("should apply interest to savings accounts")
        void applyInterestSuccess() throws Exception {
            mockMvc.perform(post("/api/accounts/apply-interest"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.accountsUpdated", greaterThanOrEqualTo(0)))
                    .andExpect(jsonPath("$.message", containsString("Interest applied")));
        }
    }
}
