# Architecture Decision Records

This directory contains Architecture Decision Records (ADRs) for the Java Banking System.

## What is an ADR?

An ADR captures an important architectural decision made along with its context and consequences.

## ADR Index

| ADR | Title | Status | Date |
|-----|-------|--------|------|
| [001](001-spring-boot.md) | Use Spring Boot 3.x | Accepted | 2024-12-01 |
| [002](002-vue-tailwind.md) | Use Vue 3 + Tailwind CSS | Accepted | 2024-12-01 |
| [003](003-h2-database.md) | Use H2 In-Memory Database | Accepted | 2024-12-01 |
| [004](004-docker-compose.md) | Use Docker Compose for Deployment | Accepted | 2024-12-01 |
| [005](005-cobol-equivalence-testing.md) | COBOL Equivalence Testing Strategy | Accepted | 2024-12-01 |

## ADR Template

```markdown
# ADR-XXX: Title

## Status
Accepted | Superseded | Deprecated

## Context
What is the issue that we're seeing that is motivating this decision?

## Decision
What is the change that we're proposing and/or doing?

## Consequences
What becomes easier or more difficult to do because of this change?
```
