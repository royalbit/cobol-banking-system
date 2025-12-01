# API Documentation

## Base URL

- Development: `http://localhost:8080/api`
- Docker: `http://localhost:8080/api` (via nginx proxy at `:3000/api`)

## Interactive Documentation

- **Swagger UI**: http://localhost:8080/swagger-ui.html
- **OpenAPI Spec (live)**: http://localhost:8080/v3/api-docs
- **OpenAPI Spec (file)**: [openapi.yaml](openapi.yaml)
- **Postman Collection**: [postman-collection.json](postman-collection.json)

### Import to Postman

1. Open Postman
2. Click **Import**
3. Select `docs/api/postman-collection.json`
4. Collection will appear with all endpoints ready to use

## Authentication

Currently no authentication required (demo application).

## Endpoints

### Accounts

#### List All Accounts

```http
GET /api/accounts
```

**Response** `200 OK`
```json
[
  {
    "accountId": "ACC001",
    "customerName": "Alice Johnson",
    "balance": 5650.00,
    "accountType": "SAVINGS",
    "status": "ACTIVE"
  }
]
```

#### Get Account by ID

```http
GET /api/accounts/{accountId}
```

**Response** `200 OK`
```json
{
  "accountId": "ACC001",
  "customerName": "Alice Johnson",
  "balance": 5650.00,
  "accountType": "SAVINGS",
  "status": "ACTIVE"
}
```

**Response** `404 Not Found`
```json
{
  "error": "Account not found",
  "message": "Account with ID 'XXX' does not exist"
}
```

#### Create Account

```http
POST /api/accounts
Content-Type: application/json
```

**Request Body**
```json
{
  "accountId": "ACC006",
  "customerName": "New Customer",
  "initialBalance": 1000.00,
  "accountType": "SAVINGS"
}
```

**Response** `201 Created`
```json
{
  "accountId": "ACC006",
  "customerName": "New Customer",
  "balance": 1000.00,
  "accountType": "SAVINGS",
  "status": "ACTIVE"
}
```

#### Delete (Close) Account

```http
DELETE /api/accounts/{accountId}
```

**Response** `200 OK`
```json
{
  "accountId": "ACC001",
  "status": "INACTIVE"
}
```

### Transactions

#### Deposit

```http
POST /api/accounts/{accountId}/deposit
Content-Type: application/json
```

**Request Body**
```json
{
  "amount": 500.00
}
```

**Response** `200 OK`
```json
{
  "accountId": "ACC001",
  "balance": 6150.00,
  "message": "Deposited $500.00"
}
```

#### Withdraw

```http
POST /api/accounts/{accountId}/withdraw
Content-Type: application/json
```

**Request Body**
```json
{
  "amount": 100.00
}
```

**Response** `200 OK`
```json
{
  "accountId": "ACC001",
  "balance": 6050.00,
  "message": "Withdrew $100.00"
}
```

**Response** `400 Bad Request` (insufficient funds)
```json
{
  "error": "Insufficient funds",
  "message": "Cannot withdraw $1000.00 from balance of $500.00"
}
```

#### Apply Interest

```http
POST /api/accounts/apply-interest
```

Applies 2% interest to all SAVINGS accounts.

**Response** `200 OK`
```json
{
  "message": "Interest applied to 3 savings accounts",
  "accountsUpdated": 3
}
```

### Transaction History

#### Get All Transactions

```http
GET /api/accounts/{accountId}/transactions
```

**Response** `200 OK`
```json
[
  {
    "id": 1,
    "accountId": "ACC001",
    "type": "DEPOSIT",
    "typeLabel": "Deposit",
    "amount": 500.00,
    "date": "2024-12-01",
    "time": "14:30:00"
  }
]
```

#### Get Mini Statement

```http
GET /api/accounts/{accountId}/transactions/mini-statement
```

Returns the last 5 transactions.

**Response** `200 OK`
```json
[
  {
    "id": 5,
    "type": "WITHDRAWAL",
    "typeLabel": "Withdrawal",
    "amount": 100.00,
    "date": "2024-12-01",
    "time": "15:45:00"
  }
]
```

## Error Responses

All errors follow this format:

```json
{
  "error": "Error Type",
  "message": "Human-readable description",
  "timestamp": "2024-12-01T15:00:00Z"
}
```

| Status | Description |
|--------|-------------|
| 400 | Bad Request (validation error, insufficient funds) |
| 404 | Not Found (account doesn't exist) |
| 500 | Internal Server Error |

## Data Types

### AccountType

| Value | Description |
|-------|-------------|
| `SAVINGS` | Savings account (earns 2% interest) |
| `CHECKING` | Checking account |

### TransactionType

| Value | Label | Description |
|-------|-------|-------------|
| `DEPOSIT` | Deposit | Money added |
| `WITHDRAWAL` | Withdrawal | Money removed |
| `INTEREST` | Interest | 2% interest applied |

### AccountStatus

| Value | Description |
|-------|-------------|
| `ACTIVE` | Account is open |
| `INACTIVE` | Account is closed |
