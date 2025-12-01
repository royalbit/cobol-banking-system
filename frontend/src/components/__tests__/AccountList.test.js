import { describe, it, expect, vi } from 'vitest'
import { mount } from '@vue/test-utils'
import AccountList from '../AccountList.vue'

describe('AccountList', () => {
  const mockAccounts = [
    {
      accountId: 'ACC001',
      customerName: 'John Doe',
      balance: 1500.50,
      status: 'ACTIVE',
      accountType: 'SAVINGS'
    },
    {
      accountId: 'ACC002',
      customerName: 'Jane Smith',
      balance: 2500.00,
      status: 'ACTIVE',
      accountType: 'CHECKING'
    }
  ]

  it('renders account count', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: mockAccounts }
    })
    expect(wrapper.text()).toContain('2 account(s)')
  })

  it('displays empty state when no accounts', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: [] }
    })
    expect(wrapper.text()).toContain('No accounts yet')
    expect(wrapper.text()).toContain('Create your first account to get started')
  })

  it('renders account cards with correct information', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: mockAccounts }
    })
    expect(wrapper.text()).toContain('ACC001')
    expect(wrapper.text()).toContain('John Doe')
    expect(wrapper.text()).toContain('$1,500.50')
    expect(wrapper.text()).toContain('SAVINGS')
    expect(wrapper.text()).toContain('ACTIVE')
  })

  it('emits select event when account is clicked', async () => {
    const wrapper = mount(AccountList, {
      props: { accounts: mockAccounts }
    })
    const accountCard = wrapper.find('[class*="cursor-pointer"]')
    await accountCard.trigger('click')
    expect(wrapper.emitted('select')).toBeTruthy()
    expect(wrapper.emitted('select')[0]).toEqual([mockAccounts[0]])
  })

  it('formats currency correctly', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: mockAccounts }
    })
    expect(wrapper.text()).toContain('$1,500.50')
    expect(wrapper.text()).toContain('$2,500.00')
  })

  it('applies correct status badge styling', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: mockAccounts }
    })
    const statusBadges = wrapper.findAll('[class*="rounded-full"]')
    const activeStatus = statusBadges.find(el => el.text() === 'ACTIVE')
    expect(activeStatus.classes()).toContain('bg-green-100')
    expect(activeStatus.classes()).toContain('text-green-800')
  })

  it('applies correct type badge for savings', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: [mockAccounts[0]] }
    })
    const badges = wrapper.findAll('[class*="rounded-full"]')
    const typeBadge = badges.find(el => el.text() === 'SAVINGS')
    expect(typeBadge.classes()).toContain('bg-blue-100')
    expect(typeBadge.classes()).toContain('text-blue-800')
  })

  it('applies correct type badge for checking', () => {
    const wrapper = mount(AccountList, {
      props: { accounts: [mockAccounts[1]] }
    })
    const badges = wrapper.findAll('[class*="rounded-full"]')
    const typeBadge = badges.find(el => el.text() === 'CHECKING')
    expect(typeBadge.classes()).toContain('bg-purple-100')
    expect(typeBadge.classes()).toContain('text-purple-800')
  })
})
