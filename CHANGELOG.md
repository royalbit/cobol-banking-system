# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.2.0] - 2024-12-01

### Added
- GitHub Actions CI pipeline
- CI badges in README
- CHANGELOG.md
- CONTRIBUTING.md
- Project rebrand to "Java Banking System"

### Changed
- README updated with Asimov narrative
- warmup.yaml updated with new identity

## [1.1.0] - 2024-12-01

### Added
- Documentation folder structure (`docs/`)
- C4 Model diagrams (Context, Container, Component)
- PlantUML source files + SVG renders
- Architecture Decision Records (ADR-001 through ADR-004)
- Installation guide
- Development guide
- API reference documentation

## [1.0.0] - 2024-12-01

### Added
- Docker containerization with docker-compose
- Nginx reverse proxy for frontend
- Spring Boot Actuator for health checks
- Demo data seeding (5 sample accounts)
- Production-ready deployment

## [0.5.0] - 2024-12-01

### Added
- Vue 3 + Vite frontend
- Tailwind CSS v4 styling
- Account dashboard
- Account detail view with transactions
- Create account modal
- Deposit/withdraw forms
- Component tests (Vitest) - 29 tests
- E2E tests (Playwright) - 14 tests

## [0.4.0] - 2024-12-01

### Added
- Spring Boot REST controllers
- Account endpoints (CRUD, deposit, withdraw)
- Transaction endpoints (history, mini-statement)
- OpenAPI/Swagger documentation
- Integration tests
- JaCoCo coverage (80% minimum)

### Changed
- Total tests: 96 (was 74)

## [0.3.0] - 2024-12-01

### Added
- Transaction audit trail
- Mini statements (last 5 transactions)
- Date/time formatting (COBOL style)

### Changed
- Total tests: 74 (was 60)

## [0.2.0] - 2024-12-01

### Added
- Account CRUD operations
- Deposit/Withdrawal with validation
- Interest application (2% for savings)
- 60 equivalence tests

## [0.1.0] - 2024-12-01

### Added
- Java project scaffold (Spring Boot 3.x, Java 17)
- Domain models: Account, Transaction
- Test fixtures from COBOL data files
- Equivalence test harness
