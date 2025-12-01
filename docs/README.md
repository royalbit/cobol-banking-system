# Documentation

## Architecture

The system architecture is documented using the [C4 Model](https://c4model.com/).

| Level | Diagram | Description |
|-------|---------|-------------|
| 1 | [Context](architecture/c4-context.md) | System context and external actors |
| 2 | [Container](architecture/c4-container.md) | High-level technology choices |
| 3 | [Component - Backend](architecture/c4-component-backend.md) | Spring Boot internal structure |
| 3 | [Component - Frontend](architecture/c4-component-frontend.md) | Vue.js component structure |

## Architecture Decision Records

| ADR | Title | Status |
|-----|-------|--------|
| [001](adr/001-spring-boot.md) | Use Spring Boot 3.x | Accepted |
| [002](adr/002-vue-tailwind.md) | Use Vue 3 + Tailwind CSS | Accepted |
| [003](adr/003-h2-database.md) | Use H2 In-Memory Database | Accepted |
| [004](adr/004-docker-compose.md) | Use Docker Compose for Deployment | Accepted |

## Guides

| Guide | Description |
|-------|-------------|
| [Installation](guides/installation.md) | Setup and run locally |
| [Development](guides/development.md) | Development workflow |
| [API Reference](api/README.md) | REST API documentation |

## Quick Links

- [Main README](../README.md)
- [Changelog](../CHANGELOG.md)
- [Contributing](../CONTRIBUTING.md)
