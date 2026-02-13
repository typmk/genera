<script lang="ts">
  import { getProcessList, getSort, setSort } from '$lib/stores/processes.svelte';
  import { selectPid, getSelectedPids } from '$lib/stores/ui.svelte';
  import DataTable from '$lib/components/DataTable.svelte';
  import type { Column, Process } from '$lib/types';

  let processes = $derived(getProcessList());
  let selected = $derived(getSelectedPids());
  let sort = $derived(getSort());

  const columns: Column<Process>[] = [
    { key: 'shortName', label: 'Name', width: '180px' },
    { key: 'pid', label: 'PID', width: '70px', align: 'right' },
    { key: 'source', label: 'Source', width: '100px' },
    { key: 'state', label: 'Status', width: '80px' },
    { key: 'user', label: 'User', width: '120px' },
    { key: 'cpuPercent', label: 'CPU', width: '70px', align: 'right',
      render: (v) => `${v.toFixed(1)}%`
    },
    { key: 'memoryMb', label: 'Memory', width: '90px', align: 'right',
      render: (v) => `${v.toFixed(0)} MB`
    },
    { key: 'threads', label: 'Threads', width: '70px', align: 'right' },
    { key: 'command', label: 'Command Line', width: 'auto',
      render: (v) => `<span class="truncate font-mono" title="${v}">${v}</span>`
    },
  ];

  function handleSelect(row: Process) {
    selectPid(row.pid);
  }

  function handleSort(key: string) {
    setSort(key);
  }
</script>

<div class="details-tab">
  <DataTable
    data={processes}
    {columns}
    {selected}
    {sort}
    onSelect={handleSelect}
    onSort={handleSort}
  />
</div>

<style>
  .details-tab {
    display: flex;
    flex-direction: column;
    height: 100%;
    overflow: hidden;
  }
</style>
