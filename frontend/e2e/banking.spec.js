import { test, expect } from '@playwright/test'

test.describe('Banking Application', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/')
  })

  test('displays header with app title', async ({ page }) => {
    await expect(page.locator('h1')).toContainText('COBOL Banking System')
    await expect(page.locator('text=Modernized with Java Spring Boot')).toBeVisible()
  })

  test('shows new account button', async ({ page }) => {
    await expect(page.getByRole('button', { name: '+ New Account' })).toBeVisible()
  })

  test('shows apply interest button', async ({ page }) => {
    await expect(page.getByRole('button', { name: 'Apply Interest (2%)' })).toBeVisible()
  })

  test('opens create account modal', async ({ page }) => {
    await page.getByRole('button', { name: '+ New Account' }).click()
    await expect(page.locator('text=Create New Account')).toBeVisible()
    await expect(page.locator('input[placeholder*="ACC001"]')).toBeVisible()
    await expect(page.locator('input[placeholder*="John Doe"]')).toBeVisible()
  })

  test('closes create account modal on cancel', async ({ page }) => {
    await page.getByRole('button', { name: '+ New Account' }).click()
    await expect(page.locator('text=Create New Account')).toBeVisible()
    await page.getByRole('button', { name: 'Cancel' }).click()
    await expect(page.locator('text=Create New Account')).not.toBeVisible()
  })

  test('closes create account modal on X button', async ({ page }) => {
    await page.getByRole('button', { name: '+ New Account' }).click()
    await expect(page.locator('text=Create New Account')).toBeVisible()
    await page.locator('button:has-text("✕")').click()
    await expect(page.locator('text=Create New Account')).not.toBeVisible()
  })

  test('validates required fields on account creation', async ({ page }) => {
    await page.getByRole('button', { name: '+ New Account' }).click()
    await page.getByRole('button', { name: 'Create Account' }).click()
    await expect(page.locator('text=Please fill in all required fields')).toBeVisible()
  })

  test('shows loading state initially', async ({ page }) => {
    await page.route('**/api/accounts', async route => {
      await new Promise(resolve => setTimeout(resolve, 100))
      await route.fulfill({ json: [] })
    })
    await page.goto('/')
    await expect(page.locator('.animate-spin')).toBeVisible()
  })

  test('shows empty state when no accounts', async ({ page }) => {
    await page.route('**/api/accounts', route => route.fulfill({ json: [] }))
    await page.goto('/')
    await expect(page.locator('text=No accounts yet')).toBeVisible()
    await expect(page.locator('text=Create your first account to get started')).toBeVisible()
  })

  test('displays account cards', async ({ page }) => {
    const mockAccounts = [
      {
        accountId: 'ACC001',
        customerName: 'John Doe',
        balance: 1500.50,
        status: 'ACTIVE',
        accountType: 'SAVINGS'
      }
    ]
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.goto('/')
    await expect(page.locator('text=ACC001')).toBeVisible()
    await expect(page.locator('text=John Doe')).toBeVisible()
    await expect(page.locator('text=$1,500.50')).toBeVisible()
    await expect(page.locator('text=SAVINGS')).toBeVisible()
    await expect(page.locator('text=ACTIVE')).toBeVisible()
  })

  test('navigates to account detail on click', async ({ page }) => {
    const mockAccounts = [
      {
        accountId: 'ACC001',
        customerName: 'John Doe',
        balance: 1500.50,
        status: 'ACTIVE',
        accountType: 'SAVINGS'
      }
    ]
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: [] })
    )
    await page.goto('/')
    await page.locator('text=John Doe').click()
    await expect(page.locator('text=Back to accounts')).toBeVisible()
    await expect(page.locator('text=Current Balance')).toBeVisible()
    await expect(page.locator('text=Quick Transaction')).toBeVisible()
  })

  test('shows mini statement in account detail', async ({ page }) => {
    const mockAccounts = [
      {
        accountId: 'ACC001',
        customerName: 'John Doe',
        balance: 1500.50,
        status: 'ACTIVE',
        accountType: 'SAVINGS'
      }
    ]
    const mockTransactions = [
      { id: 1, type: 'DEPOSIT', typeLabel: 'Deposit', amount: 500, date: '2024-01-15', time: '10:30' }
    ]
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: mockTransactions })
    )
    await page.goto('/')
    await page.locator('text=John Doe').click()
    await expect(page.locator('text=Mini Statement')).toBeVisible()
    await expect(page.locator('text=Last 5 transactions')).toBeVisible()
    await expect(page.locator('p.font-medium:has-text("Deposit")')).toBeVisible()
  })

  test('navigates back to account list', async ({ page }) => {
    const mockAccounts = [
      {
        accountId: 'ACC001',
        customerName: 'John Doe',
        balance: 1500.50,
        status: 'ACTIVE',
        accountType: 'SAVINGS'
      }
    ]
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: [] })
    )
    await page.goto('/')
    await page.locator('text=John Doe').click()
    await expect(page.locator('text=Back to accounts')).toBeVisible()
    await page.locator('text=Back to accounts').click()
    await expect(page.locator('text=Accounts')).toBeVisible()
  })

  test('shows error state on API failure', async ({ page }) => {
    await page.route('**/api/accounts', route =>
      route.fulfill({ status: 500, body: 'Server Error' })
    )
    await page.goto('/')
    await expect(page.locator('text=Failed to fetch accounts')).toBeVisible()
  })
})
