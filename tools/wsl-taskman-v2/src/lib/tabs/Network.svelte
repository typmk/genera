<script lang="ts">
  import { onMount } from 'svelte';
  import { getConnections, isLoading, refreshConnections } from '$lib/stores/network.svelte';
  import DataTable from '$lib/components/DataTable.svelte';
  import type { Column, NetworkConnection } from '$lib/types';

  // Reactive data from cached store - instant!
  let connections = $derived(getConnections());
  let loading = $derived(isLoading());

  const columns: Column<NetworkConnection>[] = [
    { key: 'localAddr', label: 'Local Address', width: '140px' },
    { key: 'localPort', label: 'Port', width: '70px', align: 'right' },
    { key: 'remoteAddr', label: 'Remote Address', width: '140px' },
    { key: 'remotePort', label: 'Port', width: '70px', align: 'right' },
    { key: 'state', label: 'State', width: '100px',
      render: (v) => {
        const cls = v === 'Established' || v === 'ESTABLISHED' ? 'success' :
                    v === 'Listen' || v === 'LISTEN' ? 'info' : 'default';
        return `<span class="state-badge ${cls}">${v}</span>`;
      }
    },
    { key: 'protocol', label: 'Protocol', width: '70px' },
    { key: 'processName', label: 'Process', width: '120px' },
    { key: 'pid', label: 'PID', width: '70px', align: 'right' },
  ];

  // Refresh data when tab becomes visible (but use cache if recent)
  onMount(() => {
    refreshConnections();
  });

  function handleRefresh() {
    refreshConnections(true); // Force refresh
  }
</script>

<div class="network-tab">
  <div class="toolbar">
    <button class="btn" onclick={handleRefresh} disabled={loading}>
      {loading ? '...' : '↻'} Refresh
    </button>
    <span class="count">{connections.length} connections</span>
  </div>

  <DataTable data={connections} {columns} rowKey="id" />
</div>

<style>
  .network-tab {
    display: flex;
    flex-direction: column;
    height: 100%;
    overflow: hidden;
  }

  .toolbar {
    display: flex;
    align-items: center;
    gap: var(--space-3);
    padding: var(--space-3) var(--space-4);
    background: var(--surface-raised);
    border-bottom: 1px solid var(--border-subtle);
  }

  .btn {
    padding: var(--space-2) var(--space-3);
    background: var(--surface-overlay);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-md);
    color: var(--text-primary);
    font-size: var(--text-sm);
    cursor: pointer;
  }

  .btn:hover:not(:disabled) {
    background: var(--surface-hover);
  }

  .btn:disabled {
    opacity: 0.5;
    cursor: not-allowed;
  }

  .count {
    color: var(--text-secondary);
    font-size: var(--text-sm);
  }

  :global(.state-badge) {
    display: inline-block;
    padding: 2px 6px;
    font-size: var(--text-xs);
    font-weight: 500;
    border-radius: var(--radius-sm);
  }

  :global(.state-badge.success) {
    background: oklch(0.65 0.15 145 / 0.2);
    color: var(--color-success);
  }

  :global(.state-badge.info) {
    background: oklch(0.65 0.12 220 / 0.2);
    color: var(--color-info);
  }

  :global(.state-badge.default) {
    background: var(--surface-active);
    color: var(--text-muted);
  }
</style>
