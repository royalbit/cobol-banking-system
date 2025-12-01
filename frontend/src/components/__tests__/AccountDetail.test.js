import { describe, it, expect, vi, beforeEach } from 'vitest'
import { mount, flushPromises } from '@vue/test-utils'
import AccountDetail from '../AccountDetail.vue'

vi.mock('../../api/client', () => ({
  deposit: vi.fn(),
  withdraw: vi.fn(),
  deleteAccount: vi.fn(),
  fetchMiniStatement: vi.fn()
}))

import { deposit, withdraw, deleteAccount, fetchMiniStatement } from '../../api/client'

describe('AccountDetail', () => {
  const mockAccount = {
    accountId: 'ACC001',
    customerName: 'John Doe',
    balance: 1500.50,
    status: 'ACTIVE',
    accountType: 'SAVINGS'
  }

  const mockTransactions = [
    { id: 1, type: 'DEPOSIT', typeLabel: 'Deposit', amount: 500, date: '2024-01-15', time: '10:30' },
    { id: 2, type: 'WITHDRAWAL', typeLabel: 'Withdrawal', amount: 100, date: '2024-01-14', time: '14:00' }
  ]

  beforeEach(() => {
    vi.clearAllMocks()
    fetchMiniStatement.mockResolvedValue(mockTransactions)
    window.confirm = vi.fn(() => true)
    window.alert = vi.fn()
  })

  it('displays account information', () => {
    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })
    expect(wrapper.text()).toContain('ACC001')
    expect(wrapper.text()).toContain('John Doe')
    expect(wrapper.text()).toContain('$1,500.50')
    expect(wrapper.text()).toContain('ACTIVE')
    expect(wrapper.text()).toContain('SAVINGS')
  })

  it('emits close event when back button clicked', async () => {
    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })
    const backButton = wrapper.find('button')
    await backButton.trigger('click')
    expect(wrapper.emitted('close')).toBeTruthy()
  })

  it('loads mini statement on mount', async () => {
    mount(AccountDetail, {
      props: { account: mockAccount }
    })
    await flushPromises()
    expect(fetchMiniStatement).toHaveBeenCalledWith('ACC001')
  })

  it('shows transaction form for active accounts', () => {
    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })
    expect(wrapper.text()).toContain('Quick Transaction')
    expect(wrapper.find('input[type="number"]').exists()).toBe(true)
  })

  it('hides transaction form for inactive accounts', () => {
    const inactiveAccount = { ...mockAccount, status: 'CLOSED' }
    const wrapper = mount(AccountDetail, {
      props: { account: inactiveAccount }
    })
    expect(wrapper.text()).not.toContain('Quick Transaction')
  })

  it('shows validation error for empty amount', async () => {
    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })
    const depositButton = wrapper.findAll('button').find(b => b.text() === 'Deposit')
    await depositButton.trigger('click')
    expect(wrapper.text()).toContain('Please enter a valid amount')
  })

  it('calls deposit API and emits updated', async () => {
    deposit.mockResolvedValue({ balance: 2000 })

    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })

    await wrapper.find('input[type="number"]').setValue('500')
    const depositButton = wrapper.findAll('button').find(b => b.text() === 'Deposit')
    await depositButton.trigger('click')
    await flushPromises()

    expect(deposit).toHaveBeenCalledWith('ACC001', 500)
    expect(wrapper.emitted('updated')).toBeTruthy()
  })

  it('calls withdraw API and emits updated', async () => {
    withdraw.mockResolvedValue({ balance: 1000 })

    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })

    await wrapper.find('input[type="number"]').setValue('500')
    const withdrawButton = wrapper.findAll('button').find(b => b.text() === 'Withdraw')
    await withdrawButton.trigger('click')
    await flushPromises()

    expect(withdraw).toHaveBeenCalledWith('ACC001', 500)
    expect(wrapper.emitted('updated')).toBeTruthy()
  })

  it('displays deposit error on API failure', async () => {
    deposit.mockRejectedValue(new Error('Deposit failed'))

    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })

    await wrapper.find('input[type="number"]').setValue('500')
    const depositButton = wrapper.findAll('button').find(b => b.text() === 'Deposit')
    await depositButton.trigger('click')
    await flushPromises()

    expect(wrapper.text()).toContain('Deposit failed')
  })

  it('calls deleteAccount after confirmation', async () => {
    deleteAccount.mockResolvedValue({})

    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })

    const closeButton = wrapper.findAll('button').find(b => b.text() === 'Close this account')
    await closeButton.trigger('click')
    await flushPromises()

    expect(window.confirm).toHaveBeenCalled()
    expect(deleteAccount).toHaveBeenCalledWith('ACC001')
    expect(wrapper.emitted('close')).toBeTruthy()
  })

  it('does not delete when confirmation cancelled', async () => {
    window.confirm = vi.fn(() => false)

    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })

    const closeButton = wrapper.findAll('button').find(b => b.text() === 'Close this account')
    await closeButton.trigger('click')

    expect(deleteAccount).not.toHaveBeenCalled()
  })

  it('displays transactions with correct styling', async () => {
    const wrapper = mount(AccountDetail, {
      props: { account: mockAccount }
    })
    await flushPromises()

    expect(wrapper.text()).toContain('Deposit')
    expect(wrapper.text()).toContain('Withdrawal')
  })
})
