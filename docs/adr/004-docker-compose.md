# ADR-004: Use Docker Compose for Deployment

## Status

Accepted

## Context

We need a deployment strategy that:
- Works consistently across environments
- Bundles frontend and backend together
- Is easy to run with a single command
- Doesn't require cloud infrastructure

Options considered:
1. **Docker Compose** - Multi-container orchestration
2. **Single Docker image** - Monolithic container
3. **Kubernetes** - Production orchestration (overkill)
4. **Manual deployment** - Run JAR + npm separately

## Decision

Use **Docker Compose** with separate containers for frontend and backend.

Architecture:
```
┌─────────────────────────────────────────┐
│           docker-compose.yml            │
├─────────────────┬───────────────────────┤
│    frontend     │       backend         │
│  (Nginx:3000)   │  (Spring Boot:8080)   │
│                 │                       │
│  Vue 3 SPA      │  REST API + H2        │
│  Proxy /api/*   │  Actuator health      │
└─────────────────┴───────────────────────┘
```

## Consequences

### Positive
- Single `docker-compose up` starts everything
- Nginx handles static files efficiently
- API proxying built into Nginx config
- Health checks ensure proper startup order
- Easy to add more services later

### Negative
- Requires Docker installed
- Two containers for a simple demo
- Build time for both images

### Container Details

| Service | Base Image | Port | Purpose |
|---------|------------|------|---------|
| frontend | nginx:alpine | 3000→80 | Serve SPA, proxy API |
| backend | eclipse-temurin:17-jre-alpine | 8080 | REST API, database |

### Health Checks

- Backend: `/actuator/health` endpoint
- Frontend depends on backend being healthy
- 30s interval, 3 retries, 40s start period
