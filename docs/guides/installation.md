# Installation Guide

## Quick Start (Docker)

The fastest way to run the complete system:

```bash
# Clone the repository
git clone https://github.com/royalbit/cobol-banking-system.git
cd cobol-banking-system

# Start with Docker Compose
docker-compose up --build

# Access the application
# Frontend: http://localhost:3000
# Backend API: http://localhost:8080/api
# Swagger UI: http://localhost:8080/swagger-ui.html
# H2 Console: http://localhost:8080/h2-console
```

## Prerequisites

### For Docker Deployment
- Docker 20.x or later
- Docker Compose v2.x or later

### For Local Development
- Java 17 (OpenJDK recommended)
- Node.js 20.x or later
- npm 10.x or later

## Local Development Setup

### Backend (Spring Boot)

```bash
cd java-banking

# Run tests
./gradlew test

# Start the server
./gradlew bootRun

# The API will be available at http://localhost:8080
```

### Frontend (Vue 3)

```bash
cd frontend

# Install dependencies
npm install

# Start development server
npm run dev

# The UI will be available at http://localhost:3000
```

## Verify Installation

### Check Backend Health

```bash
curl http://localhost:8080/actuator/health
# Expected: {"status":"UP"}
```

### Check API

```bash
curl http://localhost:8080/api/accounts
# Expected: JSON array of demo accounts
```

### Check Frontend

Open http://localhost:3000 in your browser. You should see the banking dashboard with 5 demo accounts.

## Environment Configuration

### Backend (`application.properties`)

| Property | Default | Description |
|----------|---------|-------------|
| `server.port` | 8080 | API server port |
| `spring.h2.console.enabled` | true | Enable H2 web console |
| `app.demo-data.enabled` | true | Seed demo accounts on startup |

### Frontend (`vite.config.js`)

| Property | Default | Description |
|----------|---------|-------------|
| `server.port` | 3000 | Dev server port |
| `server.proxy./api` | localhost:8080 | Backend API proxy |

## Troubleshooting

### Port Already in Use

```bash
# Find process using port 8080
lsof -i :8080

# Kill the process
kill -9 <PID>
```

### Java Version Issues

```bash
# Check Java version
java -version

# Should show: openjdk version "17.x.x"
# If not, install Java 17:
# macOS: brew install openjdk@17
# Ubuntu: sudo apt install openjdk-17-jdk
```

### Node Version Issues

```bash
# Check Node version
node -v

# Should show: v20.x.x or later
# Use nvm to manage versions:
nvm install 20
nvm use 20
```
