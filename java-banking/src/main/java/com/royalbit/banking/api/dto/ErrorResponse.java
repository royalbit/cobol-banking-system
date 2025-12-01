package com.royalbit.banking.api.dto;

import java.time.Instant;

/**
 * Standard error response DTO.
 */
public record ErrorResponse(
        int status,
        String error,
        String message,
        String path,
        Instant timestamp
) {
    public static ErrorResponse of(int status, String error, String message, String path) {
        return new ErrorResponse(status, error, message, path, Instant.now());
    }
}
