package com.royalbit.banking.domain;

/**
 * Transaction type matching COBOL TRANS-TYPE field.
 * D = Deposit, W = Withdrawal, I = Interest, X = Delete
 *
 * Display labels match COBOL MINI-STATEMENT output format.
 */
public enum TransactionType {
    DEPOSIT('D', "DEP"),
    WITHDRAWAL('W', "WTH"),
    INTEREST('I', "INT"),
    DELETE('X', "DEL");

    private final char code;
    private final String displayLabel;

    TransactionType(char code, String displayLabel) {
        this.code = code;
        this.displayLabel = displayLabel;
    }

    public char getCode() {
        return code;
    }

    /**
     * Get COBOL-style display label for mini statements.
     * COBOL: DEP for deposit, WTH for withdrawal, INT for interest, DEL for delete
     */
    public String getDisplayLabel() {
        return displayLabel;
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
