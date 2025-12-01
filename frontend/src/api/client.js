const API_BASE = '/api';

export async function fetchAccounts() {
  const res = await fetch(`${API_BASE}/accounts`);
  if (!res.ok) throw new Error('Failed to fetch accounts');
  return res.json();
}

export async function fetchAccount(accountId) {
  const res = await fetch(`${API_BASE}/accounts/${accountId}`);
  if (!res.ok) throw new Error('Account not found');
  return res.json();
}

export async function createAccount(data) {
  const res = await fetch(`${API_BASE}/accounts`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(data),
  });
  if (!res.ok) {
    const error = await res.json().catch(() => ({}));
    throw new Error(error.message || 'Failed to create account');
  }
  return res.json();
}

export async function deposit(accountId, amount) {
  const res = await fetch(`${API_BASE}/accounts/${accountId}/deposit`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ amount }),
  });
  if (!res.ok) throw new Error('Deposit failed');
  return res.json();
}

export async function withdraw(accountId, amount) {
  const res = await fetch(`${API_BASE}/accounts/${accountId}/withdraw`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ amount }),
  });
  if (!res.ok) {
    if (res.status === 400) throw new Error('Insufficient funds');
    throw new Error('Withdrawal failed');
  }
  return res.json();
}

export async function deleteAccount(accountId) {
  const res = await fetch(`${API_BASE}/accounts/${accountId}`, {
    method: 'DELETE',
  });
  if (!res.ok) throw new Error('Failed to delete account');
  return res.json();
}

export async function fetchTransactions(accountId) {
  const res = await fetch(`${API_BASE}/accounts/${accountId}/transactions`);
  if (!res.ok) throw new Error('Failed to fetch transactions');
  return res.json();
}

export async function fetchMiniStatement(accountId) {
  const res = await fetch(`${API_BASE}/accounts/${accountId}/transactions/mini-statement`);
  if (!res.ok) throw new Error('Failed to fetch mini statement');
  return res.json();
}

export async function applyInterest() {
  const res = await fetch(`${API_BASE}/accounts/apply-interest`, {
    method: 'POST',
  });
  if (!res.ok) throw new Error('Failed to apply interest');
  return res.json();
}
