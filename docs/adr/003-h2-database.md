# ADR-003: Use H2 In-Memory Database

## Status

Accepted

## Context

We need a database for storing accounts and transactions that:
- Works out of the box with no setup
- Integrates with Spring Data JPA
- Supports SQL for familiarity
- Is suitable for a demo application

Options considered:
1. **H2 In-Memory** - Zero config, embedded
2. **PostgreSQL** - Production-grade, requires setup
3. **SQLite** - File-based, simple
4. **MongoDB** - Document store, different paradigm

## Decision

Use **H2 Database in in-memory mode**.

Rationale:
- Zero configuration required
- Embedded in the application (no separate process)
- Full SQL support with JDBC
- Web console for debugging (`/h2-console`)
- Perfect for demos and development
- Spring Boot auto-configures everything

## Consequences

### Positive
- Instant startup, no database setup
- Data resets on restart (clean slate for demos)
- H2 Console for visual inspection
- Identical JPA code works with PostgreSQL later

### Negative
- Data is not persisted across restarts
- Not suitable for production workloads
- Single-connection limitations

### Mitigations
- DataInitializer seeds demo data on startup
- Clear documentation that this is for demo purposes
- JPA abstraction allows easy migration to PostgreSQL

### Future Migration Path

To switch to PostgreSQL:
1. Add `postgresql` dependency
2. Update `application.properties` with connection string
3. Change `ddl-auto` from `create-drop` to `validate`
4. No code changes required (JPA abstraction)
