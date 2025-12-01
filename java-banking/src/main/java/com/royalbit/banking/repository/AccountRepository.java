package com.royalbit.banking.repository;

import com.royalbit.banking.domain.Account;
import com.royalbit.banking.domain.AccountStatus;
import com.royalbit.banking.domain.AccountType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface AccountRepository extends JpaRepository<Account, String> {

    List<Account> findByStatus(AccountStatus status);

    List<Account> findByAccountType(AccountType accountType);

    List<Account> findByAccountTypeAndStatus(AccountType accountType, AccountStatus status);
}
