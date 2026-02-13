// ============================================
// Network Store - Cached connections for instant loading
// ============================================

import type { NetworkConnection } from '$lib/types';
import { getNetworkConnections as fetchConnections } from '$lib/tauri';

// Cached connection data
let connections = $state<NetworkConnection[]>([]);
let loading = $state(false);
let lastFetch = $state(0);

// Cache duration (5 seconds)
const CACHE_DURATION = 5000;

// ============================================
// Getters
// ============================================

export function getConnections(): NetworkConnection[] {
  return connections;
}

export function isLoading(): boolean {
  return loading;
}

// ============================================
// Actions
// ============================================

export async function refreshConnections(force = false): Promise<void> {
  const now = Date.now();

  // Skip if recently fetched (unless forced)
  if (!force && connections.length > 0 && now - lastFetch < CACHE_DURATION) {
    return;
  }

  // Skip if already loading
  if (loading) return;

  loading = true;
  try {
    const data = await fetchConnections();
    connections = data;
    lastFetch = now;
  } catch (e) {
    console.error('Failed to fetch connections:', e);
  }
  loading = false;
}

// Pre-fetch connections on module load (runs once when imported)
refreshConnections();
