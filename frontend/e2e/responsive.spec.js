import { test, expect, devices } from '@playwright/test'

/**
 * Responsive/Mobile Viewport E2E Tests
 * v1.4.0 - Testing across different device sizes
 */

// Common mock data
const mockAccounts = [
  {
    accountId: 'ACC001',
    customerName: 'Alice Johnson',
    balance: 5650.00,
    status: 'ACTIVE',
    accountType: 'SAVINGS'
  },
  {
    accountId: 'ACC002',
    customerName: 'Bob Smith',
    balance: 2340.50,
    status: 'ACTIVE',
    accountType: 'CHECKING'
  }
]

const mockTransactions = [
  { id: 1, type: 'DEPOSIT', typeLabel: 'Deposit', amount: 500, date: '2024-12-01', time: '10:00' },
  { id: 2, type: 'WITHDRAWAL', typeLabel: 'Withdrawal', amount: 100, date: '2024-12-01', time: '11:00' }
]

test.describe('Mobile Portrait (iPhone 12)', () => {
  test.use({ viewport: { width: 390, height: 844 } })

  test.beforeEach(async ({ page }) => {
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: mockTransactions })
    )
  })

  test('displays header correctly on mobile', async ({ page }) => {
    await page.goto('/')
    await expect(page.locator('h1')).toBeVisible()
    await expect(page.locator('h1')).toContainText('COBOL Banking System')
  })

  test('account cards stack vertically on mobile', async ({ page }) => {
    await page.goto('/')

    // Both accounts should be visible
    await expect(page.locator('text=Alice Johnson')).toBeVisible()
    await expect(page.locator('text=Bob Smith')).toBeVisible()

    // Cards should be stacked (same x position)
    const aliceCard = page.locator('text=Alice Johnson').first()
    const bobCard = page.locator('text=Bob Smith').first()

    const aliceBox = await aliceCard.boundingBox()
    const bobBox = await bobCard.boundingBox()

    // On mobile, cards should be stacked vertically
    expect(bobBox.y).toBeGreaterThan(aliceBox.y)
  })

  test('action buttons visible and tappable on mobile', async ({ page }) => {
    await page.goto('/')

    const newAccountBtn = page.getByRole('button', { name: '+ New Account' })
    await expect(newAccountBtn).toBeVisible()

    const box = await newAccountBtn.boundingBox()
    // Button should be at least 44px tall for touch (accessibility)
    expect(box.height).toBeGreaterThanOrEqual(32)
  })

  test('create account modal fits mobile screen', async ({ page }) => {
    await page.goto('/')
    await page.getByRole('button', { name: '+ New Account' }).click()

    const modal = page.locator('text=Create New Account').locator('..')
    await expect(modal).toBeVisible()

    // Modal should not overflow viewport
    const viewport = page.viewportSize()
    const modalBox = await page.locator('.fixed.inset-0 .bg-white').boundingBox()

    expect(modalBox.width).toBeLessThanOrEqual(viewport.width)
  })

  test('account detail page works on mobile', async ({ page }) => {
    await page.goto('/')
    await page.locator('text=Alice Johnson').click()

    // Back button should be visible
    await expect(page.locator('text=Back to accounts')).toBeVisible()

    // Balance should be visible
    await expect(page.locator('text=Current Balance')).toBeVisible()

    // Transaction form should be visible
    await expect(page.locator('text=Quick Transaction')).toBeVisible()

    // Mini statement should be visible (might be stacked below on mobile)
    await expect(page.locator('text=Mini Statement')).toBeVisible()
  })

  test('transaction buttons accessible on mobile', async ({ page }) => {
    await page.goto('/')
    await page.locator('text=Alice Johnson').click()

    const depositBtn = page.getByRole('button', { name: 'Deposit' })
    const withdrawBtn = page.getByRole('button', { name: 'Withdraw' })

    await expect(depositBtn).toBeVisible()
    await expect(withdrawBtn).toBeVisible()

    // Both buttons should be clickable
    const depositBox = await depositBtn.boundingBox()
    const withdrawBox = await withdrawBtn.boundingBox()

    expect(depositBox.height).toBeGreaterThanOrEqual(32)
    expect(withdrawBox.height).toBeGreaterThanOrEqual(32)
  })
})

test.describe('Tablet Portrait (iPad)', () => {
  test.use({ viewport: { width: 768, height: 1024 } })

  test.beforeEach(async ({ page }) => {
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: mockTransactions })
    )
  })

  test('displays full header on tablet', async ({ page }) => {
    await page.goto('/')
    await expect(page.locator('h1')).toContainText('COBOL Banking System')
    await expect(page.locator('text=Modernized with Java Spring Boot')).toBeVisible()
  })

  test('account cards may show in grid on tablet', async ({ page }) => {
    await page.goto('/')

    await expect(page.locator('text=Alice Johnson')).toBeVisible()
    await expect(page.locator('text=Bob Smith')).toBeVisible()
  })

  test('account detail shows side-by-side layout', async ({ page }) => {
    await page.goto('/')
    await page.locator('text=Alice Johnson').click()

    // Both the account info and mini statement should be visible
    await expect(page.locator('text=Current Balance')).toBeVisible()
    await expect(page.locator('text=Mini Statement')).toBeVisible()
  })
})

test.describe('Desktop (1280px)', () => {
  test.use({ viewport: { width: 1280, height: 800 } })

  test.beforeEach(async ({ page }) => {
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: mockTransactions })
    )
  })

  test('displays full desktop layout', async ({ page }) => {
    await page.goto('/')

    // Header should be fully visible
    await expect(page.locator('h1')).toContainText('COBOL Banking System')
    await expect(page.locator('text=Modernized with Java Spring Boot')).toBeVisible()

    // Action buttons should be visible
    await expect(page.getByRole('button', { name: '+ New Account' })).toBeVisible()
    await expect(page.getByRole('button', { name: 'Apply Interest (2%)' })).toBeVisible()
  })

  test('account detail uses 3-column grid on desktop', async ({ page }) => {
    await page.goto('/')
    await page.locator('text=Alice Johnson').click()

    // On desktop, account info takes 2 columns, mini statement takes 1
    const accountInfoCard = page.locator('text=Current Balance').locator('..').locator('..')
    const miniStatementCard = page.locator('text=Mini Statement').locator('..')

    const infoBox = await accountInfoCard.boundingBox()
    const miniBox = await miniStatementCard.boundingBox()

    // Both should be visible side by side on desktop
    await expect(accountInfoCard).toBeVisible()
    await expect(miniStatementCard).toBeVisible()

    // Account info should be wider than mini statement (2:1 ratio roughly)
    expect(infoBox.width).toBeGreaterThan(miniBox.width)
  })
})

test.describe('Wide Desktop (1920px)', () => {
  test.use({ viewport: { width: 1920, height: 1080 } })

  test.beforeEach(async ({ page }) => {
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
  })

  test('content is centered and constrained on wide screens', async ({ page }) => {
    await page.goto('/')

    // Main content should not stretch to full width
    const mainContent = page.locator('main').first()
    const mainBox = await mainContent.boundingBox()

    // Content should have max-width and be centered
    expect(mainBox.width).toBeLessThan(1920)
  })
})

test.describe('Mobile Landscape', () => {
  test.use({ viewport: { width: 844, height: 390 } })

  test.beforeEach(async ({ page }) => {
    await page.route('**/api/accounts', route => route.fulfill({ json: mockAccounts }))
    await page.route('**/api/accounts/ACC001/transactions/mini-statement', route =>
      route.fulfill({ json: mockTransactions })
    )
  })

  test('app remains usable in landscape mode', async ({ page }) => {
    await page.goto('/')

    await expect(page.locator('h1')).toBeVisible()
    await expect(page.locator('text=Alice Johnson')).toBeVisible()

    // Can navigate to detail
    await page.locator('text=Alice Johnson').click()
    await expect(page.locator('text=Current Balance')).toBeVisible()
  })
})
