# ADR-005: COBOL Equivalence Testing Strategy

## Status

Accepted

## Context

The Java Banking System was originally conceived as a modernization of a COBOL banking application (`BANKACCT.cob`). To ensure the Java implementation maintains functional equivalence with the original COBOL behavior, we need a comprehensive testing strategy that verifies identical behavior across all banking operations.

### Original COBOL System

The COBOL program (`BANKACCT.cob`, 389 lines) implements:

| Operation | COBOL Paragraph | Key Logic |
|-----------|-----------------|-----------|
| Create Account | `CREATE-ACCOUNT` | Write to CUSTOMER-FILE |
| View Accounts | `VIEW-ACCOUNTS` | Read all from CUSTOMER-FILE |
| Deposit | `DEPOSIT-MONEY` | `ADD WS-AMOUNT TO BALANCE` |
| Withdraw | `WITHDRAW-MONEY` | `IF BALANCE >= WS-AMOUNT` then subtract |
| Mini Statement | `MINI-STATEMENT` | Last 5 transactions |
| Apply Interest | `APPLY-INTEREST` | `COMPUTE WS-AMOUNT = BALANCE * 0.02` |

### Data Formats

COBOL data definitions that must be matched:

```cobol
05 ACCT-ID     PIC X(10)      -- Account ID (10 chars)
05 NAME        PIC X(30)      -- Customer name (30 chars, space-padded)
05 BALANCE     PIC 9(7)V99    -- Balance (max 9,999,999.99, 2 decimal places)
05 ACCT-TYPE   PIC X(1)       -- Type: 'S' = Savings, 'C' = Checking

05 TRANS-TYPE  PIC X(1)       -- D=Deposit, W=Withdrawal, I=Interest, X=Delete
05 TRANS-DATE  PIC X(10)      -- Format: YYYY/MM/DD
05 TRANS-TIME  PIC X(8)       -- Format: HH:MM:SS
```

## Decision

We implement a multi-layered equivalence testing strategy with the following test categories:

### 1. Arithmetic Precision Tests

Verify that Java `BigDecimal` operations match COBOL `PIC 9(7)V99` behavior:

- **Addition**: `ADD WS-AMOUNT TO BALANCE`
- **Subtraction**: `SUBTRACT WS-AMOUNT FROM BALANCE`
- **Multiplication**: `COMPUTE WS-AMOUNT = BALANCE * 0.02`
- **Scale**: Exactly 2 decimal places
- **Maximum**: 9,999,999.99

### 2. Business Rule Tests

Verify identical behavior for all banking operations:

| Operation | COBOL Behavior | Java Verification |
|-----------|----------------|-------------------|
| Deposit | Add to balance, log 'D' | `BankingService.deposit()` |
| Withdraw | Check balance >= amount, subtract, log 'W' | `BankingService.withdraw()` |
| Interest | Only savings ('S'), 2% calculation, log 'I' | `BankingService.applyInterest()` |
| Delete | Soft delete, log 'X' | `BankingService.deleteAccount()` |
| Mini Statement | Exactly 5 transactions max | `BankingService.getMiniStatement()` |

### 3. Edge Case Tests

Boundary conditions that must behave identically:

- Zero balance operations
- Zero amount transactions
- Exact balance withdrawal
- Maximum balance (9,999,999.99)
- Insufficient funds rejection
- Inactive account operations

### 4. Scenario Replay Tests

Multi-operation sequences that verify cumulative state:

```
Scenario: Create → Deposit × 3 → Withdraw → Interest → Verify
Expected: Final balance matches COBOL calculation exactly
```

### 5. Data Format Tests

Verify output formats match COBOL:

- Date format: `YYYY/MM/DD`
- Time format: `HH:MM:SS`
- Name padding: 30 characters, space-padded
- Transaction codes: D, W, I, X

## Implementation

### Test Classes

```
src/test/java/com/royalbit/banking/
├── CobolEquivalenceTest.java    # Comprehensive equivalence tests
├── EquivalenceTest.java         # Original data loading tests
└── TestFixtures.java            # COBOL data fixtures
```

### Test Counts by Category

| Category | Tests |
|----------|-------|
| Arithmetic Precision | 6 |
| Deposit Operations | 4 |
| Withdraw Operations | 5 |
| Interest Calculation | 5 |
| Mini Statement | 4 |
| Create Account | 6 |
| Transaction Logging | 3 |
| Edge Cases | 6 |
| Scenario Replay | 6 |
| **Total New** | **45** |

### Verification Approach

1. **Static Verification**: Compare Java output against known COBOL output
2. **Calculation Verification**: Verify arithmetic matches COBOL COMPUTE/ADD/SUBTRACT
3. **State Verification**: Verify final state after operation sequences
4. **Format Verification**: Verify date/time/name formatting matches COBOL

## Consequences

### Positive

- **High Confidence**: Comprehensive tests ensure functional equivalence
- **Regression Prevention**: Any drift from COBOL behavior is immediately detected
- **Documentation**: Tests serve as executable specification of COBOL behavior
- **Maintainability**: Clear test categories make it easy to add new equivalence tests

### Negative

- **Test Maintenance**: Tests must be updated if intentional divergence from COBOL is needed
- **Complexity**: Some COBOL-specific behaviors may be difficult to replicate exactly
- **No Runtime COBOL**: We verify against specification, not live COBOL execution

### Risks Mitigated

- **Arithmetic Drift**: BigDecimal scale/rounding verified against COBOL PIC 9(7)V99
- **Logic Drift**: Business rules verified against COBOL IF/COMPUTE statements
- **Format Drift**: Date/time/name formats verified against COBOL STRING operations

## References

- `BANKACCT.cob` - Original COBOL source (389 lines)
- `CUSTOMERS.DAT` - COBOL customer data file
- `TRANSACTIONS.DAT` - COBOL transaction log file
- `TestFixtures.java` - Java representation of COBOL data
