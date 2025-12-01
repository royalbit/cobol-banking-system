package com.royalbit.banking.domain;

/**
 * Account status matching COBOL status field.
 * A = Active, I = Inactive
 */
public enum AccountStatus {
    ACTIVE('A'),
    INACTIVE('I');

    private final char code;

    AccountStatus(char code) {
        this.code = code;
    }

    public char getCode() {
        return code;
    }

    public static AccountStatus fromCode(char code) {
        for (AccountStatus status : values()) {
            if (status.code == code) {
                return status;
            }
        }
        throw new IllegalArgumentException("Unknown account status code: " + code);
    }
}
