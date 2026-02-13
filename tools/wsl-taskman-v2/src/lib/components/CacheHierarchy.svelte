<script lang="ts">
  import { invoke } from '@tauri-apps/api/core';
  import { onMount } from 'svelte';

  interface CacheInfo {
    level: number;
    cacheType: string;
    sizeKb: number;
    lineSize: number;
    associativity: number;
    sets: number;
    sharedByCores: number;
  }

  interface CpuTopology {
    vendor: string;
    brand: string;
    physicalCores: number;
    logicalCores: number;
    caches: CacheInfo[];
  }

  interface MemoryFlow {
    pageFaultsPerSec: number;
    cacheBytes: number;
    cacheFaultsPerSec: number;
    commitBytes: number;
    availableBytes: number;
  }

  interface DiskFlow {
    name: string;
    readBytesPerSec: number;
    writeBytesPerSec: number;
    queueLength: number;
  }

  let topology: CpuTopology | null = $state(null);
  let memoryFlow: MemoryFlow | null = $state(null);
  let diskFlows: DiskFlow[] = $state([]);
  let loading = $state(true);

  // Group caches by level
  let l1Data = $derived(topology?.caches.filter(c => c.level === 1 && c.cacheType === 'Data') ?? []);
  let l1Inst = $derived(topology?.caches.filter(c => c.level === 1 && c.cacheType === 'Instruction') ?? []);
  let l2 = $derived(topology?.caches.filter(c => c.level === 2) ?? []);
  let l3 = $derived(topology?.caches.filter(c => c.level === 3) ?? []);

  function formatSize(kb: number): string {
    if (kb >= 1024) {
      return `${(kb / 1024).toFixed(0)} MB`;
    }
    return `${kb} KB`;
  }

  function formatBytes(bytes: number): string {
    if (bytes >= 1024 * 1024 * 1024) {
      return `${(bytes / 1024 / 1024 / 1024).toFixed(1)} GB`;
    }
    if (bytes >= 1024 * 1024) {
      return `${(bytes / 1024 / 1024).toFixed(0)} MB`;
    }
    if (bytes >= 1024) {
      return `${(bytes / 1024).toFixed(0)} KB`;
    }
    return `${bytes} B`;
  }

  function formatSpeed(bytesPerSec: number): string {
    if (bytesPerSec >= 1024 * 1024 * 1024) {
      return `${(bytesPerSec / 1024 / 1024 / 1024).toFixed(1)} GB/s`;
    }
    if (bytesPerSec >= 1024 * 1024) {
      return `${(bytesPerSec / 1024 / 1024).toFixed(0)} MB/s`;
    }
    return `${(bytesPerSec / 1024).toFixed(0)} KB/s`;
  }

  async function loadData() {
    loading = true;
    try {
      const [topo, mem, disk] = await Promise.all([
        invoke<CpuTopology>('get_cache_topology'),
        invoke<MemoryFlow>('get_memory_flow').catch(() => null),
        invoke<DiskFlow[]>('get_disk_flow').catch(() => []),
      ]);
      topology = topo;
      memoryFlow = mem;
      diskFlows = disk;
    } catch (e) {
      console.error('Failed to load cache topology:', e);
    }
    loading = false;
  }

  onMount(() => {
    loadData();
    // Refresh memory/disk flow every 2 seconds
    const interval = setInterval(async () => {
      try {
        memoryFlow = await invoke<MemoryFlow>('get_memory_flow');
        diskFlows = await invoke<DiskFlow[]>('get_disk_flow');
      } catch {}
    }, 2000);
    return () => clearInterval(interval);
  });
</script>

{#if loading}
  <div class="loading">Loading cache topology...</div>
{:else if topology}
  <div class="hierarchy">
    <!-- CPU Brand Header -->
    <div class="cpu-header">
      <span class="vendor">{topology.vendor}</span>
      <span class="brand">{topology.brand}</span>
      <span class="cores">{topology.physicalCores}C / {topology.logicalCores}T</span>
    </div>

    <!-- Cache Hierarchy Diagram -->
    <div class="cache-diagram">
      <!-- L1 Caches - one per core -->
      <div class="cache-row l1-row">
        {#each { length: Math.min(topology.physicalCores, 6) } as _, i}
          <div class="core-caches">
            <span class="core-label">Core {i}</span>
            <div class="l1-pair">
              <div class="cache-box l1d" title="L1 Data Cache">
                <span class="type">L1d</span>
                <span class="size">{l1Data[0] ? formatSize(l1Data[0].sizeKb) : '?'}</span>
              </div>
              <div class="cache-box l1i" title="L1 Instruction Cache">
                <span class="type">L1i</span>
                <span class="size">{l1Inst[0] ? formatSize(l1Inst[0].sizeKb) : '?'}</span>
              </div>
            </div>
          </div>
        {/each}
        {#if topology.physicalCores > 6}
          <div class="more-cores">+{topology.physicalCores - 6} more</div>
        {/if}
      </div>

      <!-- Connector -->
      <div class="connector"></div>

      <!-- L2 Caches -->
      <div class="cache-row l2-row">
        {#each { length: Math.min(topology.physicalCores, 6) } as _, i}
          <div class="cache-box l2" title="L2 Unified Cache">
            <span class="type">L2</span>
            <span class="size">{l2[0] ? formatSize(l2[0].sizeKb) : '?'}</span>
          </div>
        {/each}
        {#if topology.physicalCores > 6}
          <div class="more-cores"></div>
        {/if}
      </div>

      <!-- Connector -->
      <div class="connector wide"></div>

      <!-- L3 Cache (shared) -->
      {#if l3.length > 0}
        <div class="cache-row l3-row">
          <div class="cache-box l3" title="L3 Shared Cache">
            <span class="type">L3 Shared</span>
            <span class="size">{formatSize(l3[0].sizeKb)}</span>
            <span class="detail">{l3[0].associativity}-way, {l3[0].lineSize}B line</span>
          </div>
        </div>
        <div class="connector wide"></div>
      {/if}

      <!-- Memory -->
      <div class="memory-row">
        <div class="memory-box">
          <span class="type">RAM</span>
          {#if memoryFlow}
            <span class="stats">
              Cache: {formatBytes(memoryFlow.cacheBytes)}
            </span>
            <span class="flow">
              Page Faults: {memoryFlow.pageFaultsPerSec.toFixed(0)}/s
            </span>
          {:else}
            <span class="size">System Memory</span>
          {/if}
        </div>
      </div>

      <div class="connector wide"></div>

      <!-- Disk -->
      <div class="disk-row">
        {#each diskFlows as disk}
          <div class="disk-box">
            <span class="name">{disk.name}</span>
            <span class="io read">R: {formatSpeed(disk.readBytesPerSec)}</span>
            <span class="io write">W: {formatSpeed(disk.writeBytesPerSec)}</span>
            {#if disk.queueLength > 0}
              <span class="queue">Q: {disk.queueLength}</span>
            {/if}
          </div>
        {:else}
          <div class="disk-box">
            <span class="name">Storage</span>
            <span class="size">Disk I/O</span>
          </div>
        {/each}
      </div>
    </div>

    <!-- Cache Details Table -->
    <div class="cache-details">
      <h4>Cache Details</h4>
      <table>
        <thead>
          <tr>
            <th>Level</th>
            <th>Type</th>
            <th>Size</th>
            <th>Line Size</th>
            <th>Assoc</th>
            <th>Sets</th>
          </tr>
        </thead>
        <tbody>
          {#each topology.caches as cache}
            <tr>
              <td>L{cache.level}</td>
              <td>{cache.cacheType}</td>
              <td>{formatSize(cache.sizeKb)}</td>
              <td>{cache.lineSize}B</td>
              <td>{cache.associativity}-way</td>
              <td>{cache.sets}</td>
            </tr>
          {/each}
        </tbody>
      </table>
    </div>
  </div>
{:else}
  <div class="error">Failed to load cache topology</div>
{/if}

<style>
  .hierarchy {
    display: flex;
    flex-direction: column;
    gap: var(--space-4);
  }

  .loading, .error {
    padding: var(--space-4);
    text-align: center;
    color: var(--text-muted);
  }

  .cpu-header {
    display: flex;
    align-items: center;
    gap: var(--space-3);
    padding: var(--space-2) var(--space-3);
    background: var(--surface-base);
    border-radius: var(--radius-md);
  }

  .vendor {
    font-size: var(--text-xs);
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .brand {
    flex: 1;
    font-size: var(--text-sm);
    font-weight: 500;
    color: var(--text-primary);
  }

  .cores {
    font-size: var(--text-sm);
    color: var(--accent);
    font-weight: 600;
  }

  .cache-diagram {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: var(--space-1);
    padding: var(--space-4);
    background: var(--surface-base);
    border-radius: var(--radius-lg);
  }

  .cache-row {
    display: flex;
    gap: var(--space-2);
    justify-content: center;
    flex-wrap: wrap;
  }

  .core-caches {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: var(--space-1);
  }

  .core-label {
    font-size: var(--text-xs);
    color: var(--text-muted);
  }

  .l1-pair {
    display: flex;
    gap: var(--space-1);
  }

  .cache-box {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: var(--space-2) var(--space-3);
    border-radius: var(--radius-sm);
    min-width: 50px;
    text-align: center;
    transition: transform 0.1s, box-shadow 0.1s;
    cursor: default;
  }

  .cache-box:hover {
    transform: scale(1.05);
    box-shadow: 0 2px 8px oklch(0 0 0 / 0.2);
  }

  .cache-box .type {
    font-size: var(--text-xs);
    font-weight: 600;
    opacity: 0.8;
  }

  .cache-box .size {
    font-size: var(--text-sm);
    font-weight: 500;
  }

  .cache-box .detail {
    font-size: var(--text-xs);
    opacity: 0.7;
  }

  .l1d {
    background: linear-gradient(135deg, oklch(0.6 0.15 25), oklch(0.5 0.12 25));
    color: white;
  }

  .l1i {
    background: linear-gradient(135deg, oklch(0.6 0.15 200), oklch(0.5 0.12 200));
    color: white;
  }

  .l2 {
    background: linear-gradient(135deg, oklch(0.55 0.12 145), oklch(0.45 0.1 145));
    color: white;
    min-width: 70px;
  }

  .l3 {
    background: linear-gradient(135deg, oklch(0.5 0.1 280), oklch(0.4 0.08 280));
    color: white;
    min-width: 200px;
    padding: var(--space-3) var(--space-6);
  }

  .connector {
    width: 2px;
    height: 12px;
    background: var(--border-default);
  }

  .connector.wide {
    width: 60%;
    height: 2px;
  }

  .memory-row, .disk-row {
    display: flex;
    gap: var(--space-2);
    justify-content: center;
    flex-wrap: wrap;
  }

  .memory-box {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: var(--space-3) var(--space-6);
    background: linear-gradient(135deg, oklch(0.45 0.08 220), oklch(0.35 0.06 220));
    color: white;
    border-radius: var(--radius-md);
    min-width: 200px;
  }

  .memory-box .type {
    font-size: var(--text-sm);
    font-weight: 600;
  }

  .memory-box .stats, .memory-box .flow {
    font-size: var(--text-xs);
    opacity: 0.9;
  }

  .disk-box {
    display: flex;
    flex-direction: column;
    align-items: center;
    padding: var(--space-2) var(--space-4);
    background: linear-gradient(135deg, oklch(0.4 0.05 60), oklch(0.3 0.04 60));
    color: white;
    border-radius: var(--radius-md);
    min-width: 120px;
  }

  .disk-box .name {
    font-size: var(--text-xs);
    font-weight: 600;
    max-width: 150px;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .disk-box .io {
    font-size: var(--text-xs);
    opacity: 0.9;
  }

  .disk-box .io.read {
    color: oklch(0.8 0.15 145);
  }

  .disk-box .io.write {
    color: oklch(0.8 0.15 25);
  }

  .disk-box .queue {
    font-size: var(--text-xs);
    color: oklch(0.8 0.15 60);
  }

  .more-cores {
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: var(--text-xs);
    color: var(--text-muted);
    min-width: 50px;
  }

  .cache-details {
    margin-top: var(--space-2);
  }

  .cache-details h4 {
    font-size: var(--text-sm);
    font-weight: 600;
    color: var(--text-secondary);
    margin-bottom: var(--space-2);
  }

  .cache-details table {
    width: 100%;
    font-size: var(--text-sm);
    border-collapse: collapse;
  }

  .cache-details th, .cache-details td {
    padding: var(--space-2) var(--space-3);
    text-align: left;
    border-bottom: 1px solid var(--border-subtle);
  }

  .cache-details th {
    font-weight: 500;
    color: var(--text-secondary);
    background: var(--surface-base);
  }

  .cache-details td {
    color: var(--text-primary);
    font-variant-numeric: tabular-nums;
  }
</style>
