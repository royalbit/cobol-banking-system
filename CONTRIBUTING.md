# Contributing

Thank you for your interest in the Java Banking System!

## AI-Only Development Model

**Pull Requests are disabled.** This is intentional.

This project uses the **AI-Only Development Model** as defined in [ADR-011](https://github.com/royalbit/asimov/blob/main/docs/adr/011-ai-only-development-no-external-prs.md).

### Why No PRs?

External PRs are an **attack vector for ethics bypass**. The trust model is:

```
Human Owner → AI (autonomous) → Tests Pass → Direct Commit → Main
```

PRs require human code review, but that's not the RoyalBit Asimov model. Tests and `asimov.yaml` are the gatekeepers—not human reviewers who can be fooled by obfuscated code.

### How to Contribute

| Method | Description |
|--------|-------------|
| **[Issues](https://github.com/royalbit/cobol-banking-system/issues)** | Report bugs, request features |
| **Fork** | Create your own version |

When AI implements your idea from an Issue, you'll be credited in the commit message.

## Reporting Issues

### Bug Reports

Please include:
- Description of the bug
- Steps to reproduce
- Expected behavior
- Actual behavior
- Environment (OS, Java version, Node version)

### Feature Requests

Please include:
- Description of the feature
- Use case / motivation
- Possible implementation approach (optional)

## Code Style

If you fork the project:

### Backend (Java)
- Follow Google Java Style Guide
- Use constructor injection (not field injection)
- DTOs for API request/response
- Service layer for business logic

### Frontend (Vue/JavaScript)
- Vue 3 Composition API with `<script setup>`
- Tailwind CSS utility classes
- Single-file components (SFC)
- API calls in `src/api/client.js`

## Testing Requirements

All changes must pass:

```bash
# Backend (96 tests, 80%+ coverage)
cd java-banking && ./gradlew test jacocoTestCoverageVerification

# Frontend component tests (29 tests)
cd frontend && npm test

# Frontend E2E tests (14 tests)
cd frontend && npm run test:e2e
```

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
