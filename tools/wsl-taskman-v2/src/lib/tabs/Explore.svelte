<script lang="ts">
  import { getCores, getStats } from '$lib/stores/system.svelte';
  import { getTopMemory, getProcesses } from '$lib/stores/processes.svelte';
  import CacheHierarchy from '$lib/components/CacheHierarchy.svelte';

  let cores = $derived(getCores());
  let stats = $derived(getStats());
  let topMemory = $derived(getTopMemory(10));

  // Color scale for heatmap
  function getHeatColor(usage: number): string {
    if (usage > 75) return 'var(--color-danger)';
    if (usage > 50) return 'var(--color-warning)';
    if (usage > 25) return 'oklch(0.55 0.12 145)';
    return 'var(--surface-active)';
  }

  // Calculate total memory for percentage
  let totalMemory = $derived(stats?.memory.totalMb ?? 1);
</script>

<div class="explore-tab">
  <!-- Memory Hierarchy -->
  <section class="explore-section">
    <header>
      <h3>Memory Hierarchy</h3>
      <span class="subtitle">CPU Cache / RAM / Disk Flow</span>
    </header>
    <CacheHierarchy />
  </section>

  <!-- CPU Architecture -->
  <section class="explore-section">
    <header>
      <h3>CPU Architecture</h3>
      <span class="subtitle">{cores.length} cores</span>
    </header>
    <div class="cpu-heatmap">
      {#each cores as core, i}
        <div
          class="core-cell"
          style:background={getHeatColor(core.usage)}
        >
          <span class="core-id">Core {i}</span>
          <span class="core-usage">{core.usage.toFixed(0)}%</span>
          {#if core.topProcess}
            <span class="core-process">{core.topProcess}</span>
          {/if}
        </div>
      {/each}
    </div>
  </section>

  <!-- Memory Layout -->
  <section class="explore-section">
    <header>
      <h3>Memory Layout</h3>
      <span class="subtitle">
        {((stats?.memory.usedMb ?? 0) / 1024).toFixed(1)} / {((stats?.memory.totalMb ?? 0) / 1024).toFixed(1)} GB
      </span>
    </header>
    <div class="memory-bar">
      {#each topMemory as proc}
        {@const pct = (proc.memoryMb / totalMemory) * 100}
        {#if pct > 0.5}
          <div
            class="mem-block"
            style:width="{pct}%"
            style:background="oklch(0.55 0.15 {(proc.pid % 360)})"
            title="{proc.shortName}: {proc.memoryMb.toFixed(0)} MB"
          >
            {#if pct > 5}
              <span>{proc.shortName}</span>
            {/if}
          </div>
        {/if}
      {/each}
      <div class="mem-block free" style:flex="1">Free</div>
    </div>

    <div class="memory-breakdown">
      {#each topMemory as proc}
        <div class="mem-consumer">
          <div
            class="color-dot"
            style:background="oklch(0.55 0.15 {(proc.pid % 360)})"
          ></div>
          <span class="name">{proc.shortName}</span>
          <span class="size">{proc.memoryMb.toFixed(0)} MB</span>
        </div>
      {/each}
    </div>
  </section>

  <!-- Tools -->
  <section class="explore-section">
    <header>
      <h3>Profiling Tools</h3>
    </header>
    <div class="tools-grid">
      <button class="tool-btn">
        <span class="icon">📈</span>
        <span>perf</span>
      </button>
      <button class="tool-btn">
        <span class="icon">🔍</span>
        <span>strace</span>
      </button>
      <button class="tool-btn">
        <span class="icon">📚</span>
        <span>ltrace</span>
      </button>
      <button class="tool-btn">
        <span class="icon">🧪</span>
        <span>valgrind</span>
      </button>
      <button class="tool-btn windows">
        <span class="icon">🪟</span>
        <span>ProcMon</span>
      </button>
      <button class="tool-btn windows">
        <span class="icon">⚡</span>
        <span>xperf</span>
      </button>
    </div>
  </section>
</div>

<style>
  .explore-tab {
    padding: var(--space-4);
    overflow: auto;
    display: flex;
    flex-direction: column;
    gap: var(--space-4);
  }

  .explore-section {
    background: var(--surface-raised);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-lg);
    padding: var(--space-4);
  }

  .explore-section header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: var(--space-4);
  }

  .explore-section h3 {
    font-size: var(--text-sm);
    font-weight: 600;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .subtitle {
    font-size: var(--text-sm);
    color: var(--text-muted);
  }

  /* CPU Heatmap */
  .cpu-heatmap {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(80px, 1fr));
    gap: var(--space-2);
  }

  .core-cell {
    padding: var(--space-3);
    border-radius: var(--radius-md);
    display: flex;
    flex-direction: column;
    gap: var(--space-1);
    min-height: 70px;
    transition: background 0.2s;
  }

  .core-id {
    font-size: var(--text-xs);
    font-weight: 600;
    color: var(--text-secondary);
  }

  .core-usage {
    font-size: var(--text-lg);
    font-weight: 500;
    color: var(--text-primary);
  }

  .core-process {
    font-size: var(--text-xs);
    color: var(--text-muted);
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  /* Memory Bar */
  .memory-bar {
    display: flex;
    height: 40px;
    border-radius: var(--radius-md);
    overflow: hidden;
    background: var(--surface-base);
  }

  .mem-block {
    display: flex;
    align-items: center;
    justify-content: center;
    min-width: 2px;
    font-size: var(--text-xs);
    font-weight: 500;
    color: white;
    text-shadow: 0 1px 2px oklch(0 0 0 / 0.5);
    overflow: hidden;
    transition: width 0.3s;
  }

  .mem-block.free {
    background: var(--surface-active);
    color: var(--text-muted);
    text-shadow: none;
  }

  .memory-breakdown {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(180px, 1fr));
    gap: var(--space-2);
    margin-top: var(--space-4);
  }

  .mem-consumer {
    display: flex;
    align-items: center;
    gap: var(--space-2);
    padding: var(--space-2);
    background: var(--surface-base);
    border-radius: var(--radius-sm);
  }

  .color-dot {
    width: 8px;
    height: 24px;
    border-radius: 2px;
  }

  .mem-consumer .name {
    flex: 1;
    font-size: var(--text-sm);
    font-weight: 500;
    overflow: hidden;
    text-overflow: ellipsis;
    white-space: nowrap;
  }

  .mem-consumer .size {
    font-size: var(--text-sm);
    color: var(--text-secondary);
    font-variant-numeric: tabular-nums;
  }

  /* Tools */
  .tools-grid {
    display: flex;
    flex-wrap: wrap;
    gap: var(--space-2);
  }

  .tool-btn {
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: var(--space-1);
    padding: var(--space-3) var(--space-4);
    background: var(--surface-base);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-md);
    cursor: pointer;
    transition: all var(--transition-fast);
  }

  .tool-btn:hover {
    background: var(--surface-hover);
    border-color: var(--border-default);
  }

  .tool-btn .icon {
    font-size: 20px;
  }

  .tool-btn span:last-child {
    font-size: var(--text-sm);
    font-weight: 500;
    color: var(--text-secondary);
  }

  .tool-btn.windows {
    border-color: oklch(0.5 0.1 220 / 0.3);
  }

  .tool-btn.windows:hover {
    background: oklch(0.5 0.1 220 / 0.1);
    border-color: oklch(0.5 0.1 220 / 0.5);
  }
</style>
