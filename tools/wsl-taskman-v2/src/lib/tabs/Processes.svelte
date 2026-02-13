<script lang="ts">
  import { getProcessList, getSort, setSort } from '$lib/stores/processes.svelte';
  import { selectPid, getSelectedPids, showContextMenu, hideContextMenu, getContextMenu } from '$lib/stores/ui.svelte';
  import { killProcess, setPriority } from '$lib/tauri';
  import DataTable from '$lib/components/DataTable.svelte';
  import ContextMenu from '$lib/components/ContextMenu.svelte';
  import Badge from '$lib/components/Badge.svelte';
  import type { Column, MenuItem, Process } from '$lib/types';

  // Reactive data
  let processes = $derived(getProcessList());
  let selected = $derived(getSelectedPids());
  let sort = $derived(getSort());
  let contextMenuState = $derived(getContextMenu());

  // Column definitions
  const columns: Column<Process>[] = [
    {
      key: 'shortName',
      label: 'Name',
      width: '200px',
      render: (val, row) => `<span class="process-name">${val}</span>`
    },
    {
      key: 'pid',
      label: 'PID',
      width: '80px',
      align: 'right'
    },
    {
      key: 'cpuPercent',
      label: 'CPU',
      width: '80px',
      align: 'right',
      render: (val) => {
        const pct = val.toFixed(1);
        const cls = val > 50 ? 'high' : val > 20 ? 'med' : '';
        return `<span class="metric ${cls}">${pct}%</span>`;
      }
    },
    {
      key: 'memoryMb',
      label: 'Memory',
      width: '100px',
      align: 'right',
      render: (val) => {
        const cls = val > 500 ? 'high' : val > 200 ? 'med' : '';
        return `<span class="metric ${cls}">${val.toFixed(0)} MB</span>`;
      }
    },
    {
      key: 'state',
      label: 'Status',
      width: '100px',
      render: (val) => {
        const variant = val === 'Running' ? 'success' : 'default';
        return `<span class="badge-${variant}">${val}</span>`;
      }
    },
    {
      key: 'user',
      label: 'User',
      width: '120px'
    }
  ];

  // Context menu items
  function getMenuItems(): MenuItem[] {
    return [
      { label: 'End task', icon: '🛑', action: handleKill, danger: true },
      { label: 'End process tree', icon: '🌲', action: handleKillTree, danger: true },
      { type: 'divider' },
      { label: 'Set priority', icon: '📊', children: [
        { label: 'High', action: () => handlePriority('high') },
        { label: 'Above Normal', action: () => handlePriority('above_normal') },
        { label: 'Normal', action: () => handlePriority('normal') },
        { label: 'Below Normal', action: () => handlePriority('below_normal') },
        { label: 'Low', action: () => handlePriority('idle') },
      ]},
      { type: 'divider' },
      { label: 'View details', icon: '📝', action: handleViewDetails },
    ];
  }

  function handleSelect(row: Process) {
    selectPid(row.pid);
  }

  function handleContext(row: Process, x: number, y: number) {
    showContextMenu(x, y, row);
  }

  function handleSort(key: string) {
    setSort(key);
  }

  async function handleKill() {
    const pid = contextMenuState.target?.pid;
    if (pid) await killProcess(pid);
    hideContextMenu();
  }

  async function handleKillTree() {
    const pid = contextMenuState.target?.pid;
    if (pid) await killProcess(pid, true);
    hideContextMenu();
  }

  async function handlePriority(priority: string) {
    const pid = contextMenuState.target?.pid;
    if (pid) await setPriority(pid, priority);
  }

  function handleViewDetails() {
    // TODO: Navigate to details tab
    hideContextMenu();
  }
</script>

<div class="processes-tab">
  <DataTable
    data={processes}
    {columns}
    {selected}
    {sort}
    onSelect={handleSelect}
    onContext={handleContext}
    onSort={handleSort}
  />
</div>

{#if contextMenuState.show}
  <ContextMenu
    items={getMenuItems()}
    x={contextMenuState.x}
    y={contextMenuState.y}
    onClose={hideContextMenu}
  />
{/if}

<style>
  .processes-tab {
    display: flex;
    flex-direction: column;
    height: 100%;
    overflow: hidden;
  }

  :global(.process-name) {
    font-weight: 500;
  }

  :global(.metric) {
    font-variant-numeric: tabular-nums;
  }

  :global(.metric.high) {
    color: var(--color-danger);
    font-weight: 600;
  }

  :global(.metric.med) {
    color: var(--color-warning);
    font-weight: 500;
  }

  :global(.badge-success) {
    color: var(--color-success);
  }

  :global(.badge-default) {
    color: var(--text-muted);
  }
</style>
