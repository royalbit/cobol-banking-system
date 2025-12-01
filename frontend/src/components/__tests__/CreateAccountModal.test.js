import { describe, it, expect, vi, beforeEach } from 'vitest'
import { mount, flushPromises } from '@vue/test-utils'
import CreateAccountModal from '../CreateAccountModal.vue'

vi.mock('../../api/client', () => ({
  createAccount: vi.fn()
}))

import { createAccount } from '../../api/client'

describe('CreateAccountModal', () => {
  beforeEach(() => {
    vi.clearAllMocks()
  })

  it('renders the modal with form fields', () => {
    const wrapper = mount(CreateAccountModal)
    expect(wrapper.text()).toContain('Create New Account')
    expect(wrapper.text()).toContain('Account ID')
    expect(wrapper.text()).toContain('Customer Name')
    expect(wrapper.text()).toContain('Initial Balance')
    expect(wrapper.text()).toContain('Account Type')
  })

  it('emits close event when close button clicked', async () => {
    const wrapper = mount(CreateAccountModal)
    const closeButton = wrapper.find('button[class*="text-gray-400"]')
    await closeButton.trigger('click')
    expect(wrapper.emitted('close')).toBeTruthy()
  })

  it('emits close event when cancel button clicked', async () => {
    const wrapper = mount(CreateAccountModal)
    const cancelButton = wrapper.findAll('button').find(b => b.text() === 'Cancel')
    await cancelButton.trigger('click')
    expect(wrapper.emitted('close')).toBeTruthy()
  })

  it('shows error when submitting without required fields', async () => {
    const wrapper = mount(CreateAccountModal)
    const form = wrapper.find('form')
    await form.trigger('submit')
    expect(wrapper.text()).toContain('Please fill in all required fields')
  })

  it('calls createAccount and emits created on successful submit', async () => {
    createAccount.mockResolvedValue({ accountId: 'ACC001' })

    const wrapper = mount(CreateAccountModal)

    await wrapper.find('input[placeholder*="ACC001"]').setValue('ACC001')
    await wrapper.find('input[placeholder*="John Doe"]').setValue('John Doe')
    await wrapper.find('input[type="number"]').setValue('1000')

    const form = wrapper.find('form')
    await form.trigger('submit')
    await flushPromises()

    expect(createAccount).toHaveBeenCalledWith({
      accountId: 'ACC001',
      customerName: 'John Doe',
      initialBalance: 1000,
      accountType: 'SAVINGS'
    })
    expect(wrapper.emitted('created')).toBeTruthy()
  })

  it('displays error message on API failure', async () => {
    createAccount.mockRejectedValue(new Error('Account already exists'))

    const wrapper = mount(CreateAccountModal)

    await wrapper.find('input[placeholder*="ACC001"]').setValue('ACC001')
    await wrapper.find('input[placeholder*="John Doe"]').setValue('John Doe')

    const form = wrapper.find('form')
    await form.trigger('submit')
    await flushPromises()

    expect(wrapper.text()).toContain('Account already exists')
  })

  it('shows loading state during submission', async () => {
    createAccount.mockImplementation(() => new Promise(() => {}))

    const wrapper = mount(CreateAccountModal)

    await wrapper.find('input[placeholder*="ACC001"]').setValue('ACC001')
    await wrapper.find('input[placeholder*="John Doe"]').setValue('John Doe')

    const form = wrapper.find('form')
    await form.trigger('submit')

    expect(wrapper.text()).toContain('Creating...')
  })

  it('has savings as default account type', () => {
    const wrapper = mount(CreateAccountModal)
    const select = wrapper.find('select')
    expect(select.element.value).toBe('SAVINGS')
  })

  it('allows selecting checking account type', async () => {
    const wrapper = mount(CreateAccountModal)
    const select = wrapper.find('select')
    await select.setValue('CHECKING')
    expect(select.element.value).toBe('CHECKING')
  })
})
