package com.royalbit.banking.repository;

import com.royalbit.banking.domain.Transaction;
import com.royalbit.banking.domain.TransactionType;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface TransactionRepository extends JpaRepository<Transaction, Long> {

    List<Transaction> findByAccountId(String accountId);

    List<Transaction> findByAccountIdAndTransactionType(String accountId, TransactionType type);

    /**
     * Get transactions for mini statement (last N transactions for an account).
     * Orders by date desc, then time desc.
     */
    @Query("SELECT t FROM Transaction t WHERE t.accountId = :accountId " +
           "ORDER BY t.date DESC, t.time DESC")
    List<Transaction> findRecentByAccountId(String accountId, Pageable pageable);
}
