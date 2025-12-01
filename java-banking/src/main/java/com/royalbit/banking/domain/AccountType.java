package com.royalbit.banking.domain;

/**
 * Account type matching COBOL ACCT-TYPE field.
 * S = Savings, C = Checking
 */
public enum AccountType {
    SAVINGS('S'),
    CHECKING('C');

    private final char code;

    AccountType(char code) {
        this.code = code;
    }

    public char getCode() {
        return code;
    }

    public static AccountType fromCode(char code) {
        for (AccountType type : values()) {
            if (type.code == code) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown account type code: " + code);
    }
}
