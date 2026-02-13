<script lang="ts">
  import { getCpuPercent, getMemPercent, getCpuHistory, getMemHistory, getCores, getGpu, getMemUsed, getUptime, getStats } from '$lib/stores/system.svelte';
  import { getTopMemory } from '$lib/stores/processes.svelte';
  import Sparkline from '$lib/components/Sparkline.svelte';

  let cpuPercent = $derived(getCpuPercent());
  let memPercent = $derived(getMemPercent());
  let cpuHistory = $derived(getCpuHistory());
  let memHistory = $derived(getMemHistory());
  let cores = $derived(getCores());
  let gpu = $derived(getGpu());
  let memUsed = $derived(getMemUsed());
  let uptime = $derived(getUptime());
  let stats = $derived(getStats());
  let topMemory = $derived(getTopMemory(5));
</script>

<div class="performance-tab">
  <div class="perf-grid">
    <!-- CPU Card -->
    <div class="perf-card">
      <h3>CPU</h3>
      <div class="big-stat">
        <span class="value">{cpuPercent.toFixed(0)}</span>
        <span class="unit">%</span>
      </div>
      <div class="chart">
        <Sparkline values={cpuHistory} color="var(--accent)" height={80} maxValue={100} />
      </div>
      <div class="stat-row">
        <span>Cores</span>
        <strong>{cores.length || '--'}</strong>
      </div>
      <div class="stat-row">
        <span>Uptime</span>
        <strong>{uptime}</strong>
      </div>

      <!-- Per-core bars -->
      {#if cores.length > 0}
        <div class="core-grid">
          {#each cores as core}
            <div class="core-bar">
              <div class="core-fill" style:height="{core.usage}%"></div>
              <span class="core-label">{core.usage.toFixed(0)}%</span>
            </div>
          {/each}
        </div>
      {/if}
    </div>

    <!-- Memory Card -->
    <div class="perf-card">
      <h3>Memory</h3>
      <div class="big-stat">
        <span class="value">{memPercent.toFixed(0)}</span>
        <span class="unit">%</span>
      </div>
      <div class="chart">
        <Sparkline values={memHistory} color="var(--color-success)" height={80} maxValue={100} />
      </div>
      <div class="stat-row">
        <span>Used / Total</span>
        <strong>{memUsed}</strong>
      </div>
    </div>

    <!-- GPU Card -->
    <div class="perf-card">
      <h3>GPU</h3>
      {#if gpu}
        <div class="big-stat">
          <span class="value">{gpu.utilizationPercent.toFixed(0)}</span>
          <span class="unit">%</span>
        </div>
        <div class="stat-row">
          <span>Name</span>
          <strong class="truncate">{gpu.name}</strong>
        </div>
        <div class="stat-row">
          <span>Memory</span>
          <strong>{gpu.memoryUsedMb} / {gpu.memoryTotalMb} MB</strong>
        </div>
        {#if gpu.temperature}
          <div class="stat-row">
            <span>Temperature</span>
            <strong>{gpu.temperature}°C</strong>
          </div>
        {/if}
      {:else}
        <p class="no-data">No GPU detected</p>
      {/if}
    </div>

    <!-- Top Memory Consumers -->
    <div class="perf-card wide">
      <h3>Top Memory Consumers</h3>
      <div class="consumers">
        {#each topMemory as proc}
          <div class="consumer-row">
            <span class="name truncate">{proc.shortName}</span>
            <div class="bar-bg">
              <div class="bar-fill" style:width="{Math.min(proc.memoryMb / 10, 100)}%"></div>
            </div>
            <span class="mem-value">{proc.memoryMb.toFixed(0)} MB</span>
          </div>
        {/each}
      </div>
    </div>
  </div>
</div>

<style>
  .performance-tab {
    padding: var(--space-4);
    overflow: auto;
  }

  .perf-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
    gap: var(--space-4);
  }

  .perf-card {
    background: var(--surface-raised);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-lg);
    padding: var(--space-5);
  }

  .perf-card.wide {
    grid-column: 1 / -1;
  }

  .perf-card h3 {
    margin-bottom: var(--space-4);
    font-size: var(--text-sm);
    font-weight: 600;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .big-stat {
    margin-bottom: var(--space-4);
  }

  .big-stat .value {
    font-size: 48px;
    font-weight: 300;
    color: var(--text-primary);
    line-height: 1;
  }

  .big-stat .unit {
    font-size: var(--text-xl);
    color: var(--text-muted);
    margin-left: var(--space-1);
  }

  .chart {
    height: 80px;
    background: var(--surface-base);
    border-radius: var(--radius-md);
    margin-bottom: var(--space-4);
    overflow: hidden;
  }

  .stat-row {
    display: flex;
    justify-content: space-between;
    padding: var(--space-2) 0;
    font-size: var(--text-sm);
    border-bottom: 1px solid var(--border-subtle);
  }

  .stat-row:last-child {
    border-bottom: none;
  }

  .stat-row span {
    color: var(--text-secondary);
  }

  .stat-row strong {
    color: var(--text-primary);
    font-weight: 500;
  }

  .core-grid {
    display: flex;
    gap: var(--space-2);
    margin-top: var(--space-4);
    padding-top: var(--space-4);
    border-top: 1px solid var(--border-subtle);
  }

  .core-bar {
    flex: 1;
    height: 50px;
    background: var(--surface-base);
    border-radius: var(--radius-sm);
    position: relative;
    overflow: hidden;
  }

  .core-fill {
    position: absolute;
    bottom: 0;
    left: 0;
    right: 0;
    background: linear-gradient(to top, var(--accent), oklch(0.72 0.12 220 / 0.5));
    transition: height 0.3s ease;
  }

  .core-label {
    position: absolute;
    inset: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: var(--text-xs);
    font-weight: 600;
    color: var(--text-primary);
  }

  .consumers {
    display: flex;
    flex-direction: column;
    gap: var(--space-3);
  }

  .consumer-row {
    display: flex;
    align-items: center;
    gap: var(--space-3);
  }

  .consumer-row .name {
    width: 150px;
    font-size: var(--text-sm);
    font-weight: 500;
  }

  .bar-bg {
    flex: 1;
    height: 8px;
    background: var(--surface-base);
    border-radius: var(--radius-sm);
    overflow: hidden;
  }

  .bar-fill {
    height: 100%;
    background: var(--accent);
    border-radius: var(--radius-sm);
    transition: width 0.3s ease;
  }

  .mem-value {
    width: 80px;
    text-align: right;
    font-size: var(--text-sm);
    font-variant-numeric: tabular-nums;
    color: var(--text-secondary);
  }

  .no-data {
    color: var(--text-muted);
    font-size: var(--text-sm);
  }
</style>
