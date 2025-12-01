<script setup>
import { ref, onMounted } from 'vue'
import { fetchAccounts, applyInterest } from './api/client'
import AccountList from './components/AccountList.vue'
import AccountDetail from './components/AccountDetail.vue'
import CreateAccountModal from './components/CreateAccountModal.vue'

const accounts = ref([])
const selectedAccount = ref(null)
const showCreateModal = ref(false)
const loading = ref(true)
const error = ref(null)

async function loadAccounts() {
  loading.value = true
  error.value = null
  try {
    accounts.value = await fetchAccounts()
  } catch (e) {
    error.value = e.message
  } finally {
    loading.value = false
  }
}

function selectAccount(account) {
  selectedAccount.value = account
}

function closeDetail() {
  selectedAccount.value = null
  loadAccounts()
}

async function handleApplyInterest() {
  try {
    const result = await applyInterest()
    alert(result.message)
    loadAccounts()
  } catch (e) {
    alert('Failed to apply interest: ' + e.message)
  }
}

function handleAccountCreated() {
  showCreateModal.value = false
  loadAccounts()
}

onMounted(loadAccounts)
</script>

<template>
  <div class="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100">
    <!-- Header -->
    <header class="bg-white shadow-sm border-b border-gray-200">
      <div class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
        <div class="flex items-center justify-between">
          <div class="flex items-center space-x-3">
            <div class="w-10 h-10 bg-indigo-600 rounded-lg flex items-center justify-center">
              <span class="text-white font-bold text-xl">₿</span>
            </div>
            <div>
              <h1 class="text-xl font-bold text-gray-900">COBOL Banking System</h1>
              <p class="text-sm text-gray-500">Modernized with Java Spring Boot</p>
            </div>
          </div>
          <div class="flex items-center space-x-3">
            <button
              @click="handleApplyInterest"
              class="px-4 py-2 text-sm font-medium text-indigo-600 bg-indigo-50 rounded-lg hover:bg-indigo-100 transition-colors"
            >
              Apply Interest (2%)
            </button>
            <button
              @click="showCreateModal = true"
              class="px-4 py-2 text-sm font-medium text-white bg-indigo-600 rounded-lg hover:bg-indigo-700 transition-colors"
            >
              + New Account
            </button>
          </div>
        </div>
      </div>
    </header>

    <!-- Main Content -->
    <main class="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
      <div v-if="loading" class="flex items-center justify-center h-64">
        <div class="animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600"></div>
      </div>

      <div v-else-if="error" class="bg-red-50 border border-red-200 rounded-lg p-4 text-red-700">
        {{ error }}
      </div>

      <div v-else-if="selectedAccount">
        <AccountDetail
          :account="selectedAccount"
          @close="closeDetail"
          @updated="loadAccounts"
        />
      </div>

      <div v-else>
        <AccountList
          :accounts="accounts"
          @select="selectAccount"
          @refresh="loadAccounts"
        />
      </div>
    </main>

    <!-- Create Account Modal -->
    <CreateAccountModal
      v-if="showCreateModal"
      @close="showCreateModal = false"
      @created="handleAccountCreated"
    />
  </div>
</template>
