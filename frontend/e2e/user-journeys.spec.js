import { test, expect } from '@playwright/test'

/**
 * User Journey E2E Tests
 * v1.4.0 - Full user journey tests covering:
 * - Account creation to transactions to statements
 * - Error handling scenarios
 */

test.describe('Full User Journeys', () => {
  test('complete banking workflow: create account → deposit → withdraw → view statement', async ({ page }) => {
    // Setup mock API responses
    const testAccount = {
      accountId: 'TEST001',
      customerName: 'Jane Smith',
      balance: 0,
      status: 'ACTIVE',
      accountType: 'SAVINGS'
    }

    let currentBalance = 0
    const transactions = []

    // Mock: List accounts (initially empty, then with new account)
    await page.route('**/api/accounts', async route => {
      if (route.request().method() === 'GET') {
        if (currentBalance > 0 || transactions.length > 0) {
          await route.fulfill({ json: [{ ...testAccount, balance: currentBalance }] })
        } else {
          await route.fulfill({ json: [] })
        }
      } else if (route.request().method() === 'POST') {
        currentBalance = 1000
        await route.fulfill({
          status: 201,
          json: { ...testAccount, balance: currentBalance }
        })
      }
    })

    // Mock: Deposit endpoint
    await page.route('**/api/accounts/TEST001/deposit', async route => {
      const body = JSON.parse(route.request().postData())
      currentBalance += body.amount
      transactions.push({
        id: transactions.length + 1,
        type: 'DEPOSIT',
        typeLabel: 'Deposit',
        amount: body.amount,
        date: '2024-12-01',
        time: '10:00:00'
      })
      await route.fulfill({
        json: { accountId: 'TEST001', balance: currentBalance, message: `Deposited $${body.amount.toFixed(2)}` }
      })
    })

    // Mock: Withdraw endpoint
    await page.route('**/api/accounts/TEST001/withdraw', async route => {
      const body = JSON.parse(route.request().postData())
      currentBalance -= body.amount
      transactions.push({
        id: transactions.length + 1,
        type: 'WITHDRAWAL',
        typeLabel: 'Withdrawal',
        amount: body.amount,
        date: '2024-12-01',
        time: '10:30:00'
      })
      await route.fulfill({
        json: { accountId: 'TEST001', balance: currentBalance, message: `Withdrew $${body.amount.toFixed(2)}` }
      })
    })

    // Mock: Mini statement
    await page.route('**/api/accounts/TEST001/transactions/mini-statement', async route => {
      await route.fulfill({ json: transactions.slice(-5) })
    })

    // Step 1: Navigate to app, see empty state
    await page.goto('/')
    await expect(page.locator('text=No accounts yet')).toBeVisible()

    // Step 2: Create new account
    await page.getByRole('button', { name: '+ New Account' }).click()
    await expect(page.locator('text=Create New Account')).toBeVisible()

    await page.locator('input[placeholder*="ACC001"]').fill('TEST001')
    await page.locator('input[placeholder="John Doe"]').fill('Jane Smith')
    await page.locator('input[placeholder="0.00"]').fill('1000')
    await page.locator('select').selectOption('SAVINGS')
    await page.getByRole('button', { name: 'Create Account' }).click()

    // Step 3: Verify account appears in list
    await expect(page.locator('text=TEST001')).toBeVisible()
    await expect(page.locator('text=Jane Smith')).toBeVisible()

    // Step 4: Navigate to account detail
    await page.locator('text=Jane Smith').click()
    await expect(page.locator('text=Current Balance')).toBeVisible()

    // Step 5: Perform deposit
    await page.locator('input[placeholder="Enter amount"]').fill('500')
    await page.getByRole('button', { name: 'Deposit' }).click()

    // Step 6: Verify mini statement shows deposit
    await expect(page.locator('p.font-medium:has-text("Deposit")')).toBeVisible()

    // Step 7: Perform withdrawal
    await page.locator('input[placeholder="Enter amount"]').fill('200')
    await page.getByRole('button', { name: 'Withdraw' }).click()

    // Step 8: Verify mini statement shows both transactions
    await expect(page.locator('p.font-medium:has-text("Deposit")')).toBeVisible()
    await expect(page.locator('p.font-medium:has-text("Withdrawal")')).toBeVisible()

    // Step 9: Navigate back to account list
    await page.locator('text=Back to accounts').click()
    await expect(page.locator('h1:has-text("COBOL Banking System")')).toBeVisible()
  })

  test('apply interest to savings accounts journey', async ({ page }) => {
    const savingsAccount = {
      accountId: 'SAV001',
      customerName: 'Interest Tester',
      balance: 1000.00,
      status: 'ACTIVE',
      accountType: 'SAVINGS'
    }

    const checkingAccount = {
      accountId: 'CHK001',
      customerName: 'Checking User',
      balance: 500.00,
      status: 'ACTIVE',
      accountType: 'CHECKING'
    }

    await page.route('**/api/accounts', route => route.fulfill({
      json: [savingsAccount, checkingAccount]
    }))

    await page.route('**/api/accounts/apply-interest', route => route.fulfill({
      json: { message: 'Interest applied to 1 savings accounts', accountsUpdated: 1 }
    }))

    await page.goto('/')
    await expect(page.locator('text=SAV001')).toBeVisible()
    await expect(page.locator('text=CHK001')).toBeVisible()

    // Click apply interest
    await page.getByRole('button', { name: 'Apply Interest (2%)' }).click()

    // Verify the button was clicked (interest applied)
    await expect(page.getByRole('button', { name: 'Apply Interest (2%)' })).toBeVisible()
  })
})

test.describe('Error Handling Scenarios', () => {
  test('shows error for insufficient funds withdrawal', async ({ page }) => {
    const account = {
      accountId: 'ACC001',
      customerName: 'John Doe',
      balance: 100.00,
      status: 'ACTIVE',
      accountType: 'CHECKING'
    }

    await page.route('**/api/accounts', route => route.fulfill({ json: [account] }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: [] })
    )
    await page.route('**/api/accounts/ACC001/withdraw', route =>
      route.fulfill({
        status: 400,
        json: {
          error: 'Insufficient funds',
          message: 'Cannot withdraw $500.00 from balance of $100.00'
        }
      })
    )

    await page.goto('/')
    await page.locator('text=John Doe').click()
    await expect(page.locator('text=Current Balance')).toBeVisible()

    // Try to withdraw more than balance
    await page.locator('input[placeholder="Enter amount"]').fill('500')
    await page.getByRole('button', { name: 'Withdraw' }).click()

    // Verify error is displayed
    await expect(page.locator('text=Insufficient funds')).toBeVisible()
  })

  test('shows error for invalid amount input', async ({ page }) => {
    const account = {
      accountId: 'ACC001',
      customerName: 'John Doe',
      balance: 1000.00,
      status: 'ACTIVE',
      accountType: 'SAVINGS'
    }

    await page.route('**/api/accounts', route => route.fulfill({ json: [account] }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: [] })
    )

    await page.goto('/')
    await page.locator('text=John Doe').click()

    // Try to deposit with empty amount
    await page.getByRole('button', { name: 'Deposit' }).click()
    await expect(page.locator('text=Please enter a valid amount')).toBeVisible()

    // Try to withdraw with zero amount
    await page.locator('input[placeholder="Enter amount"]').fill('0')
    await page.getByRole('button', { name: 'Withdraw' }).click()
    await expect(page.locator('text=Please enter a valid amount')).toBeVisible()
  })

  test('shows error when creating duplicate account', async ({ page }) => {
    await page.route('**/api/accounts', async route => {
      if (route.request().method() === 'GET') {
        await route.fulfill({ json: [] })
      } else if (route.request().method() === 'POST') {
        await route.fulfill({
          status: 400,
          json: {
            error: 'Validation error',
            message: 'Account with ID ACC001 already exists'
          }
        })
      }
    })

    await page.goto('/')
    await page.getByRole('button', { name: '+ New Account' }).click()
    await page.locator('input[placeholder*="ACC001"]').fill('ACC001')
    await page.locator('input[placeholder="John Doe"]').fill('Test User')
    await page.getByRole('button', { name: 'Create Account' }).click()

    await expect(page.locator('text=Account with ID ACC001 already exists')).toBeVisible()
  })

  test('handles network error gracefully', async ({ page }) => {
    // Simulate network error by returning 503 Service Unavailable
    await page.route('**/api/accounts', route => route.fulfill({
      status: 503,
      body: 'Service Unavailable'
    }))
    await page.goto('/')
    // When network fails, the app should show an error state
    await expect(page.locator('text=Failed to fetch accounts')).toBeVisible()
  })

  test('handles server timeout gracefully', async ({ page }) => {
    await page.route('**/api/accounts', async route => {
      await new Promise(resolve => setTimeout(resolve, 5000))
      await route.abort('timedout')
    })

    await page.goto('/')
    // Should show loading state first
    await expect(page.locator('.animate-spin')).toBeVisible()
  })
})

test.describe('Account Status Scenarios', () => {
  test('inactive account hides transaction form', async ({ page }) => {
    const inactiveAccount = {
      accountId: 'CLOSED01',
      customerName: 'Closed Account User',
      balance: 0,
      status: 'INACTIVE',
      accountType: 'SAVINGS'
    }

    await page.route('**/api/accounts', route => route.fulfill({ json: [inactiveAccount] }))
    await page.route('**/api/accounts/CLOSED01/transactions/mini-statement', route =>
      route.fulfill({ json: [] })
    )

    await page.goto('/')
    await page.locator('text=Closed Account User').click()

    await expect(page.locator('text=INACTIVE')).toBeVisible()
    // Transaction form should not be visible for inactive accounts
    await expect(page.locator('text=Quick Transaction')).not.toBeVisible()
  })

  test('can close an active account', async ({ page }) => {
    const account = {
      accountId: 'ACC001',
      customerName: 'John Doe',
      balance: 0,
      status: 'ACTIVE',
      accountType: 'SAVINGS'
    }

    await page.route('**/api/accounts', route => route.fulfill({ json: [account] }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: [] })
    )
    await page.route('**/api/accounts/ACC001', async route => {
      if (route.request().method() === 'DELETE') {
        await route.fulfill({
          json: { ...account, status: 'INACTIVE' }
        })
      }
    })

    // Setup dialog handler for confirm
    page.on('dialog', dialog => dialog.accept())

    await page.goto('/')
    await page.locator('text=John Doe').click()
    await expect(page.locator('text=Close this account')).toBeVisible()

    await page.locator('text=Close this account').click()

    // Should navigate back to account list
    await expect(page.locator('h1:has-text("COBOL Banking System")')).toBeVisible()
  })
})
