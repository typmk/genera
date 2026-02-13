<script lang="ts">
  import { tabs, setTab, getTab } from '$lib/stores/ui.svelte';
  import { getCpuPercent, getMemPercent } from '$lib/stores/system.svelte';

  // Icons mapping
  const icons: Record<string, string> = {
    'list-tree': '📋',
    'table': '📝',
    'chart': '📊',
    'network': '🌐',
    'microscope': '🔬',
    'settings': '⚙️',
  };

  let currentTab = $derived(getTab());
  let cpuPercent = $derived(getCpuPercent());
  let memPercent = $derived(getMemPercent());
</script>

<nav class="sidebar">
  <div class="nav-items">
    {#each tabs as tab}
      <button
        class="nav-item"
        class:active={currentTab === tab.id}
        onclick={() => setTab(tab.id)}
      >
        <span class="nav-icon">{icons[tab.icon] || '📄'}</span>
        <span class="nav-label">{tab.label}</span>
      </button>
    {/each}
  </div>

  <div class="sidebar-footer">
    <div class="quick-stats">
      <div class="quick-stat">
        <span class="stat-label">CPU</span>
        <span class="stat-value" class:high={cpuPercent > 80}>{cpuPercent.toFixed(0)}%</span>
      </div>
      <div class="quick-stat">
        <span class="stat-label">Mem</span>
        <span class="stat-value" class:high={memPercent > 80}>{memPercent.toFixed(0)}%</span>
      </div>
    </div>
  </div>
</nav>

<style>
  .sidebar {
    width: var(--sidebar-width);
    height: 100%;
    display: flex;
    flex-direction: column;
    background: var(--surface-raised);
    border-right: 1px solid var(--border-subtle);
  }

  .nav-items {
    flex: 1;
    padding: var(--space-2);
    display: flex;
    flex-direction: column;
    gap: var(--space-1);
  }

  .nav-item {
    display: flex;
    align-items: center;
    gap: var(--space-3);
    padding: var(--space-3) var(--space-4);
    border: none;
    background: transparent;
    color: var(--text-secondary);
    font-size: var(--text-base);
    text-align: left;
    cursor: pointer;
    border-radius: var(--radius-md);
    transition: all var(--transition-fast);
  }

  .nav-item:hover {
    background: var(--surface-hover);
    color: var(--text-primary);
  }

  .nav-item.active {
    background: var(--accent-subtle);
    color: var(--accent);
  }

  .nav-icon {
    font-size: 16px;
    width: 20px;
    text-align: center;
  }

  .nav-label {
    font-weight: 500;
  }

  .sidebar-footer {
    padding: var(--space-4);
    border-top: 1px solid var(--border-subtle);
  }

  .quick-stats {
    display: flex;
    gap: var(--space-4);
  }

  .quick-stat {
    display: flex;
    flex-direction: column;
    gap: var(--space-1);
  }

  .stat-label {
    font-size: var(--text-xs);
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .stat-value {
    font-size: var(--text-lg);
    font-weight: 600;
    color: var(--text-primary);
    font-variant-numeric: tabular-nums;
  }

  .stat-value.high {
    color: var(--color-danger);
  }
</style>
