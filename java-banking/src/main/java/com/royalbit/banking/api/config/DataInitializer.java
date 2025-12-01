package com.royalbit.banking.api.config;

import com.royalbit.banking.domain.Account;
import com.royalbit.banking.domain.AccountType;
import com.royalbit.banking.repository.AccountRepository;
import com.royalbit.banking.service.BankingService;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.math.BigDecimal;

@Configuration
public class DataInitializer {

    private static final Logger log = LoggerFactory.getLogger(DataInitializer.class);

    @Value("${app.demo-data.enabled:false}")
    private boolean demoDataEnabled;

    @Bean
    public CommandLineRunner initDemoData(BankingService bankingService, AccountRepository accountRepository) {
        return args -> {
            if (!demoDataEnabled) {
                log.info("Demo data initialization disabled");
                return;
            }

            if (accountRepository.count() > 0) {
                log.info("Database already contains data, skipping demo data initialization");
                return;
            }

            log.info("Initializing demo data...");

            // Create demo accounts with initial balances
            createDemoAccount(bankingService, "ACC001", "Alice Johnson", AccountType.SAVINGS, new BigDecimal("5000.00"));
            createDemoAccount(bankingService, "ACC002", "Bob Smith", AccountType.CHECKING, new BigDecimal("2500.00"));
            createDemoAccount(bankingService, "ACC003", "Carol Williams", AccountType.SAVINGS, new BigDecimal("10000.00"));
            createDemoAccount(bankingService, "ACC004", "David Brown", AccountType.CHECKING, new BigDecimal("750.50"));
            createDemoAccount(bankingService, "ACC005", "Eve Davis", AccountType.SAVINGS, new BigDecimal("15000.00"));

            // Add some transactions to demo accounts
            bankingService.deposit("ACC001", new BigDecimal("500.00"));
            bankingService.deposit("ACC001", new BigDecimal("250.00"));
            bankingService.withdraw("ACC001", new BigDecimal("100.00"));

            bankingService.deposit("ACC002", new BigDecimal("1000.00"));
            bankingService.withdraw("ACC002", new BigDecimal("200.00"));

            bankingService.deposit("ACC003", new BigDecimal("2000.00"));

            log.info("Demo data initialization complete - {} accounts created", accountRepository.count());
        };
    }

    private void createDemoAccount(BankingService bankingService, String accountId, String customerName,
                                    AccountType accountType, BigDecimal initialBalance) {
        try {
            bankingService.createAccount(accountId, customerName, initialBalance, accountType);
            log.info("Created demo account: {} - {} ({})", accountId, customerName, accountType);
        } catch (Exception e) {
            log.warn("Failed to create demo account {}: {}", accountId, e.getMessage());
        }
    }
}
