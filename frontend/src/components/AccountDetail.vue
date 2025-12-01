<script setup>
import { ref, onMounted } from 'vue'
import { deposit, withdraw, deleteAccount, fetchMiniStatement } from '../api/client'

const props = defineProps({
  account: {
    type: Object,
    required: true
  }
})

const emit = defineEmits(['close', 'updated'])

const amount = ref('')
const transactions = ref([])
const loading = ref(false)
const error = ref(null)

function formatCurrency(value) {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(value)
}

async function loadMiniStatement() {
  try {
    transactions.value = await fetchMiniStatement(props.account.accountId)
  } catch (e) {
    console.error('Failed to load transactions:', e)
  }
}

async function handleDeposit() {
  if (!amount.value || parseFloat(amount.value) <= 0) {
    error.value = 'Please enter a valid amount'
    return
  }

  loading.value = true
  error.value = null
  try {
    await deposit(props.account.accountId, parseFloat(amount.value))
    amount.value = ''
    emit('updated')
    loadMiniStatement()
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
}

async function handleWithdraw() {
  if (!amount.value || parseFloat(amount.value) <= 0) {
    error.value = 'Please enter a valid amount'
    return
  }

  loading.value = true
  error.value = null
  try {
    await withdraw(props.account.accountId, parseFloat(amount.value))
    amount.value = ''
    emit('updated')
    loadMiniStatement()
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
}

async function handleDelete() {
  if (!confirm('Are you sure you want to close this account?')) return

  loading.value = true
  try {
    await deleteAccount(props.account.accountId)
    emit('close')
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
}

function getTypeColor(type) {
  switch (type) {
    case 'DEPOSIT': return 'text-green-600'
    case 'WITHDRAWAL': return 'text-red-600'
    case 'INTEREST': return 'text-blue-600'
    default: return 'text-gray-600'
  }
}

onMounted(loadMiniStatement)
</script>

<template>
  <div>
    <!-- Back button -->
    <button
      @click="emit('close')"
      class="flex items-center text-gray-600 hover:text-gray-900 mb-6"
    >
      <span class="mr-2">←</span> Back to accounts
    </button>

    <div class="grid gap-6 lg:grid-cols-3">
      <!-- Account Info Card -->
      <div class="lg:col-span-2 bg-white rounded-xl shadow-sm border border-gray-200 p-6">
        <div class="flex items-start justify-between mb-6">
          <div>
            <p class="text-sm font-medium text-gray-500">Account #{{ account.accountId }}</p>
            <h2 class="text-2xl font-bold text-gray-900">{{ account.customerName }}</h2>
          </div>
          <div class="text-right">
            <p class="text-sm text-gray-500">Current Balance</p>
            <p class="text-3xl font-bold text-gray-900">{{ formatCurrency(account.balance) }}</p>
          </div>
        </div>

        <div class="flex items-center space-x-2 mb-6">
          <span class="px-3 py-1 text-sm font-medium rounded-full"
                :class="account.status === 'ACTIVE' ? 'bg-green-100 text-green-800' : 'bg-gray-100 text-gray-800'">
            {{ account.status }}
          </span>
          <span class="px-3 py-1 text-sm font-medium rounded-full"
                :class="account.accountType === 'SAVINGS' ? 'bg-blue-100 text-blue-800' : 'bg-purple-100 text-purple-800'">
            {{ account.accountType }}
          </span>
        </div>

        <!-- Transaction Form -->
        <div v-if="account.status === 'ACTIVE'" class="border-t border-gray-200 pt-6">
          <h3 class="text-lg font-semibold text-gray-900 mb-4">Quick Transaction</h3>

          <div v-if="error" class="mb-4 p-3 bg-red-50 border border-red-200 rounded-lg text-red-700 text-sm">
            {{ error }}
          </div>

          <div class="flex items-center space-x-3">
            <div class="flex-1">
              <input
                v-model="amount"
                type="number"
                step="0.01"
                min="0"
                placeholder="Enter amount"
                class="w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-indigo-500 focus:border-indigo-500"
              />
            </div>
            <button
              @click="handleDeposit"
              :disabled="loading"
              class="px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700 disabled:opacity-50 transition-colors"
            >
              Deposit
            </button>
            <button
              @click="handleWithdraw"
              :disabled="loading"
              class="px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700 disabled:opacity-50 transition-colors"
            >
              Withdraw
            </button>
          </div>
        </div>

        <!-- Delete Account -->
        <div class="border-t border-gray-200 pt-6 mt-6">
          <button
            @click="handleDelete"
            :disabled="loading"
            class="text-red-600 hover:text-red-800 text-sm font-medium"
          >
            Close this account
          </button>
        </div>
      </div>

      <!-- Mini Statement -->
      <div class="bg-white rounded-xl shadow-sm border border-gray-200 p-6">
        <h3 class="text-lg font-semibold text-gray-900 mb-4">Mini Statement</h3>
        <p class="text-sm text-gray-500 mb-4">Last 5 transactions</p>

        <div v-if="transactions.length === 0" class="text-center py-8 text-gray-500">
          No transactions yet
        </div>

        <div v-else class="space-y-3">
          <div
            v-for="tx in transactions"
            :key="tx.id"
            class="flex items-center justify-between py-2 border-b border-gray-100 last:border-0"
          >
            <div>
              <p class="font-medium" :class="getTypeColor(tx.type)">
                {{ tx.typeLabel }}
              </p>
              <p class="text-xs text-gray-500">{{ tx.date }} {{ tx.time }}</p>
            </div>
            <p class="font-semibold" :class="getTypeColor(tx.type)">
              {{ tx.type === 'WITHDRAWAL' ? '-' : '+' }}{{ formatCurrency(tx.amount) }}
            </p>
          </div>
        </div>
      </div>
    </div>
  </div>
</template>
