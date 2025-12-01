# Java Banking System

[![CI](https://github.com/royalbit/cobol-banking-system/actions/workflows/ci.yml/badge.svg)](https://github.com/royalbit/cobol-banking-system/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> 🤖 **RoyalBit Asimov** | Claude (Opus 4.5) - Principal Autonomous AI
>
> A full-stack banking demo built with [RoyalBit Asimov](https://github.com/royalbit/asimov). 205 tests. Zero hallucinations.

**A modern full-stack banking application** demonstrating Spring Boot + Vue 3 + Tailwind CSS with comprehensive testing and Docker deployment.

## Quick Start

```bash
# Clone and start
git clone https://github.com/royalbit/cobol-banking-system.git
cd cobol-banking-system
docker-compose up --build

# Access the application
# Frontend:   http://localhost:3000
# Backend:    http://localhost:8080/api
# Swagger UI: http://localhost:8080/swagger-ui.html
```

## Features

| Feature | Description |
|---------|-------------|
| **Account Management** | Create, view, close accounts |
| **Transactions** | Deposit, withdraw with validation |
| **Interest** | 2% annual interest for savings |
| **Mini Statements** | Last 5 transactions |
| **Audit Trail** | Complete transaction history |

## Technology Stack

| Layer | Technology |
|-------|------------|
| Backend | Java 17, Spring Boot 3.x, JPA/Hibernate |
| Frontend | Vue 3, Vite, Tailwind CSS v4 |
| Database | H2 (in-memory) |
| API Docs | OpenAPI 3.0 / Swagger UI |
| Testing | JUnit 5 (140), Vitest (29), Playwright (36) |
| Container | Docker, docker-compose, Nginx |

## API Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| GET | `/api/accounts` | List all accounts |
| GET | `/api/accounts/{id}` | Get account by ID |
| POST | `/api/accounts` | Create account |
| DELETE | `/api/accounts/{id}` | Close account |
| POST | `/api/accounts/{id}/deposit` | Deposit funds |
| POST | `/api/accounts/{id}/withdraw` | Withdraw funds |
| POST | `/api/accounts/apply-interest` | Apply 2% interest |
| GET | `/api/accounts/{id}/transactions/mini-statement` | Last 5 transactions |

## Documentation

| Document | Description |
|----------|-------------|
| [Architecture](docs/architecture/README.md) | C4 diagrams (Context, Container, Component) |
| [ADRs](docs/adr/README.md) | Architecture Decision Records |
| [Installation](docs/guides/installation.md) | Setup and run locally |
| [Development](docs/guides/development.md) | Development workflow |
| [API Reference](docs/api/README.md) | REST API documentation |

## Development

```bash
# Backend
cd java-banking
./gradlew bootRun              # Start server
./gradlew test                 # Run tests (140 tests)

# Frontend
cd frontend
npm install && npm run dev     # Start dev server
npm test                       # Run tests (29 tests)
npm run test:e2e               # E2E tests (36 tests)
```

## Project Structure

```
java-banking-system/
├── java-banking/           # Spring Boot backend
│   ├── src/main/java/      # Java source
│   ├── src/test/java/      # Tests
│   └── Dockerfile
├── frontend/               # Vue 3 frontend
│   ├── src/                # Vue components
│   ├── e2e/                # Playwright tests
│   └── Dockerfile
├── docs/                   # Documentation
│   ├── architecture/       # C4 diagrams
│   ├── adr/                # Decision records
│   └── guides/             # Guides
└── docker-compose.yml
```

## Built by AI, Powered by the RoyalBit Asimov

**Claude (Opus 4.5) - Principal Autonomous AI**

This project was built autonomously using [**RoyalBit Asimov**](https://github.com/royalbit/asimov), a protocol for AI-driven software development with file-based truth and ethical constraints.

| Version | Features |
|---------|----------|
| v0.1.0-v0.3.0 | Domain models, banking operations, transactions |
| v0.4.0 | REST API + OpenAPI |
| v0.5.0 | Vue 3 + Tailwind frontend |
| v1.0.0 | Docker containerization |
| v1.1.0 | Documentation & C4 architecture |
| v1.2.0 | GitHub CI + project rebrand |
| v1.3.0 | OpenAPI spec + Postman collection |
| v1.4.0 | Enhanced E2E testing (36 tests) |
| v1.5.0 | COBOL functional equivalence testing (45 tests) |

**205 tests, 5 ADRs, zero warnings.**

## Contributing (AI-Only Development)

**Pull Requests are disabled.** This is intentional.

This project uses the **AI-Only Development Model** ([ADR-011](https://github.com/royalbit/asimov/blob/main/docs/adr/011-ai-only-development-no-external-prs.md)).

| Method | Description |
|--------|-------------|
| **[Issues](https://github.com/royalbit/cobol-banking-system/issues)** | Report bugs, request features |
| **Fork** | Create your own version |

See [CONTRIBUTING.md](CONTRIBUTING.md) for details.

## License

MIT - See [LICENSE](LICENSE)
