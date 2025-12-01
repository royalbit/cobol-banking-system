# ADR-001: Use Spring Boot 3.x

## Status

Accepted

## Context

We need a backend framework for the Java Banking System that provides:
- REST API capabilities
- Database integration (JPA/Hibernate)
- Dependency injection
- Testing support
- Production-ready features (health checks, metrics)

Options considered:
1. **Spring Boot 3.x** - Industry standard, extensive ecosystem
2. **Quarkus** - Cloud-native, fast startup
3. **Micronaut** - Compile-time DI, low memory
4. **Plain Java + JAX-RS** - Minimal dependencies

## Decision

Use **Spring Boot 3.x with Java 17**.

Rationale:
- Most widely adopted Java framework
- Excellent documentation and community support
- Spring Data JPA simplifies database operations
- Spring Boot Actuator provides health/metrics out of the box
- springdoc-openapi integrates seamlessly for API documentation
- Familiar to most Java developers

## Consequences

### Positive
- Rapid development with auto-configuration
- Large ecosystem of starters and integrations
- Easy testing with `@SpringBootTest`
- Built-in validation with `spring-boot-starter-validation`

### Negative
- Larger memory footprint than Quarkus/Micronaut
- Slower startup time (acceptable for demo)
- Runtime reflection (not ideal for native compilation)

### Mitigations
- Use H2 in-memory database to reduce complexity
- Docker containerization abstracts startup time concerns
