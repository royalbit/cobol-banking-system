<script setup>
defineProps({
  accounts: {
    type: Array,
    required: true
  }
})

const emit = defineEmits(['select', 'refresh'])

function formatCurrency(amount) {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD'
  }).format(amount)
}

function getStatusBadge(status) {
  return status === 'ACTIVE'
    ? 'bg-green-100 text-green-800'
    : 'bg-gray-100 text-gray-800'
}

function getTypeBadge(type) {
  return type === 'SAVINGS'
    ? 'bg-blue-100 text-blue-800'
    : 'bg-purple-100 text-purple-800'
}
</script>

<template>
  <div>
    <div class="flex items-center justify-between mb-6">
      <h2 class="text-2xl font-bold text-gray-900">Accounts</h2>
      <span class="text-sm text-gray-500">{{ accounts.length }} account(s)</span>
    </div>

    <div v-if="accounts.length === 0" class="bg-white rounded-xl shadow-sm border border-gray-200 p-12 text-center">
      <div class="w-16 h-16 bg-gray-100 rounded-full flex items-center justify-center mx-auto mb-4">
        <span class="text-3xl">🏦</span>
      </div>
      <h3 class="text-lg font-medium text-gray-900 mb-2">No accounts yet</h3>
      <p class="text-gray-500">Create your first account to get started</p>
    </div>

    <div v-else class="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
      <div
        v-for="account in accounts"
        :key="account.accountId"
        @click="emit('select', account)"
        class="bg-white rounded-xl shadow-sm border border-gray-200 p-6 cursor-pointer hover:shadow-md hover:border-indigo-300 transition-all"
      >
        <div class="flex items-start justify-between mb-4">
          <div>
            <p class="text-sm font-medium text-gray-500">{{ account.accountId }}</p>
            <h3 class="text-lg font-semibold text-gray-900">{{ account.customerName }}</h3>
          </div>
          <span :class="['px-2 py-1 text-xs font-medium rounded-full', getStatusBadge(account.status)]">
            {{ account.status }}
          </span>
        </div>

        <div class="flex items-end justify-between">
          <div>
            <p class="text-sm text-gray-500">Balance</p>
            <p class="text-2xl font-bold text-gray-900">{{ formatCurrency(account.balance) }}</p>
          </div>
          <span :class="['px-2 py-1 text-xs font-medium rounded-full', getTypeBadge(account.accountType)]">
            {{ account.accountType }}
          </span>
        </div>
      </div>
    </div>
  </div>
</template>
