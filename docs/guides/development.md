# Development Guide

## Project Structure

```
java-banking-system/
├── java-banking/           # Spring Boot backend
│   ├── src/main/java/      # Java source
│   ├── src/test/java/      # Tests
│   ├── build.gradle        # Gradle build
│   └── Dockerfile          # Backend container
├── frontend/               # Vue 3 frontend
│   ├── src/                # Vue source
│   ├── e2e/                # Playwright tests
│   ├── package.json        # npm config
│   └── Dockerfile          # Frontend container
├── docs/                   # Documentation
│   ├── architecture/       # C4 diagrams
│   ├── adr/                # Decision records
│   ├── api/                # API docs
│   └── guides/             # This guide
├── docker-compose.yml      # Container orchestration
└── README.md               # Project overview
```

## Development Workflow

### 1. Start Backend

```bash
cd java-banking
./gradlew bootRun
```

The backend runs on http://localhost:8080 with:
- REST API at `/api/*`
- Swagger UI at `/swagger-ui.html`
- H2 Console at `/h2-console`

### 2. Start Frontend

```bash
cd frontend
npm run dev
```

The frontend runs on http://localhost:3000 with:
- Hot module replacement
- API proxy to backend

### 3. Make Changes

- Backend changes: Gradle recompiles automatically with `bootRun`
- Frontend changes: Vite hot-reloads instantly

## Testing

### Backend Tests

```bash
cd java-banking

# Run all tests
./gradlew test

# Run with coverage report
./gradlew test jacocoTestReport

# View coverage report
open build/reports/jacoco/test/html/index.html
```

**Test Categories:**
- Unit tests: `*Test.java` in `src/test/java`
- Integration tests: `*ControllerTest.java` with `@SpringBootTest`

### Frontend Tests

```bash
cd frontend

# Run component tests (Vitest)
npm test

# Run with watch mode
npm run test:watch

# Run E2E tests (Playwright)
npm run test:e2e

# Run E2E with UI
npm run test:e2e:ui
```

## Code Style

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

## Adding Features

### New API Endpoint

1. Add method to `BankingService.java`
2. Add endpoint to controller
3. Create request/response DTOs if needed
4. Add integration test
5. Update OpenAPI annotations

### New UI Component

1. Create component in `src/components/`
2. Import in `App.vue` or parent
3. Add component test in `__tests__/`
4. Add E2E test if user-facing

## Database

### H2 Console Access

1. Go to http://localhost:8080/h2-console
2. JDBC URL: `jdbc:h2:mem:bankingdb`
3. Username: `sa`
4. Password: (empty)

### Schema

```sql
-- Accounts table
CREATE TABLE accounts (
    account_id VARCHAR(10) PRIMARY KEY,
    customer_name VARCHAR(30),
    balance DECIMAL(15,2),
    account_type VARCHAR(10),
    status VARCHAR(10)
);

-- Transactions table
CREATE TABLE transactions (
    id BIGINT AUTO_INCREMENT PRIMARY KEY,
    account_id VARCHAR(10),
    type VARCHAR(20),
    amount DECIMAL(15,2),
    date DATE,
    time TIME
);
```

## Useful Commands

```bash
# Backend
./gradlew clean build       # Full rebuild
./gradlew bootJar           # Build JAR
./gradlew dependencies      # Show dependencies

# Frontend
npm run build               # Production build
npm run preview             # Preview production build
npm run lint                # Lint code (if configured)

# Docker
docker-compose up --build   # Rebuild and start
docker-compose down         # Stop containers
docker-compose logs -f      # Follow logs
```
