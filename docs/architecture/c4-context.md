# C4 Model - Level 1: System Context

The System Context diagram shows the Java Banking System and its relationship with users and external systems.

![System Context Diagram](c4-context.svg)

## Elements

| Element | Type | Description |
|---------|------|-------------|
| Bank Customer | Person | End user who manages their bank accounts |
| Bank Admin | Person | Administrator who manages accounts and applies interest |
| Java Banking System | System | Full-stack banking application |
| Web Browser | External System | User's browser (Chrome, Firefox, Safari, Edge) |

## Interactions

1. **Users → Browser**: Both customers and admins access the system through a web browser
2. **Browser → Banking System**: HTTPS connection delivering the Vue SPA and REST API calls

## Key Decisions

- Single web application serving both customers and admins
- No external integrations (demo system)
- All data stored locally in H2 database

---

[← Back to Architecture](README.md) | [Next: Container Diagram →](c4-container.md)
