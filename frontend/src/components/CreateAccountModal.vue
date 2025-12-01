<script setup>
import { ref } from 'vue'
import { createAccount } from '../api/client'

const emit = defineEmits(['close', 'created'])

const form = ref({
  accountId: '',
  customerName: '',
  initialBalance: '',
  accountType: 'SAVINGS'
})

const loading = ref(false)
const error = ref(null)

async function handleSubmit() {
  error.value = null

  if (!form.value.accountId || !form.value.customerName) {
    error.value = 'Please fill in all required fields'
    return
  }

  loading.value = true
  try {
    await createAccount({
      accountId: form.value.accountId,
      customerName: form.value.customerName,
      initialBalance: parseFloat(form.value.initialBalance) || 0,
      accountType: form.value.accountType
    })
    emit('created')
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
}
</script>

<template>
  <div class="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
    <div class="bg-white rounded-xl shadow-xl max-w-md w-full p-6">
      <div class="flex items-center justify-between mb-6">
        <h2 class="text-xl font-bold text-gray-900">Create New Account</h2>
        <button @click="emit('close')" class="text-gray-400 hover:text-gray-600">
          ✕
        </button>
      </div>

      <form @submit.prevent="handleSubmit" class="space-y-4">
        <div v-if="error" class="p-3 bg-red-50 border border-red-200 rounded-lg text-red-700 text-sm">
          {{ error }}
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-1">Account ID *</label>
          <input
            v-model="form.accountId"
            type="text"
            maxlength="10"
            placeholder="e.g., ACC001"
            class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
          />
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-1">Customer Name *</label>
          <input
            v-model="form.customerName"
            type="text"
            placeholder="John Doe"
            class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
          />
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-1">Initial Balance</label>
          <input
            v-model="form.initialBalance"
            type="number"
            step="0.01"
            min="0"
            placeholder="0.00"
            class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
          />
        </div>

        <div>
          <label class="block text-sm font-medium text-gray-700 mb-1">Account Type</label>
          <select
            v-model="form.accountType"
            class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
          >
            <option value="SAVINGS">Savings (2% interest)</option>
            <option value="CHECKING">Checking</option>
          </select>
        </div>

        <div class="flex items-center justify-end space-x-3 pt-4">
          <button
            type="button"
            @click="emit('close')"
            class="px-4 py-2 text-gray-700 bg-gray-100 rounded-lg hover:bg-gray-200 transition-colors"
          >
            Cancel
          </button>
          <button
            type="submit"
            :disabled="loading"
            class="px-4 py-2 text-white bg-indigo-600 rounded-lg hover:bg-indigo-700 disabled:opacity-50 transition-colors"
          >
            {{ loading ? 'Creating...' : 'Create Account' }}
          </button>
        </div>
      </form>
    </div>
  </div>
</template>
