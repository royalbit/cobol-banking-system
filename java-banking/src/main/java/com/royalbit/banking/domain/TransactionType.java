package com.royalbit.banking.domain;

/**
 * Transaction type matching COBOL TRANS-TYPE field.
 * D = Deposit, W = Withdrawal, I = Interest, X = Delete
 */
public enum TransactionType {
    DEPOSIT('D'),
    WITHDRAWAL('W'),
    INTEREST('I'),
    DELETE('X');

    private final char code;

    TransactionType(char code) {
        this.code = code;
    }

    public char getCode() {
        return code;
    }

    public static TransactionType fromCode(char code) {
        for (TransactionType type : values()) {
            if (type.code == code) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown transaction type code: " + code);
    }
}
