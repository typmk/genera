<script lang="ts">
  import { getSettings, updateSettings } from '$lib/stores/ui.svelte';
  import { restartPolling } from '$lib/tauri';

  let settings = $derived(getSettings());

  function handleRefreshChange(event: Event) {
    const value = parseInt((event.target as HTMLSelectElement).value);
    updateSettings({ refreshRate: value });
    restartPolling();
  }

  function handleToggle(key: keyof typeof settings) {
    updateSettings({ [key]: !settings[key] });
  }
</script>

<div class="settings-tab">
  <section class="settings-section">
    <h3>General</h3>

    <div class="setting-row">
      <div class="setting-info">
        <span class="setting-label">Refresh interval</span>
        <span class="setting-desc">How often to update process data</span>
      </div>
      <select value={settings.refreshRate} onchange={handleRefreshChange}>
        <option value={1000}>1 second</option>
        <option value={2000}>2 seconds</option>
        <option value={5000}>5 seconds</option>
        <option value={10000}>10 seconds</option>
      </select>
    </div>
  </section>

  <section class="settings-section">
    <h3>Display</h3>

    <div class="setting-row">
      <div class="setting-info">
        <span class="setting-label">Compact mode</span>
        <span class="setting-desc">Reduce row height for more items</span>
      </div>
      <label class="toggle">
        <input
          type="checkbox"
          checked={settings.compactMode}
          onchange={() => handleToggle('compactMode')}
        />
        <span class="toggle-track"></span>
      </label>
    </div>

    <div class="setting-row">
      <div class="setting-info">
        <span class="setting-label">Show system processes</span>
        <span class="setting-desc">Include Windows system processes</span>
      </div>
      <label class="toggle">
        <input
          type="checkbox"
          checked={settings.showSystemProcesses}
          onchange={() => handleToggle('showSystemProcesses')}
        />
        <span class="toggle-track"></span>
      </label>
    </div>
  </section>

  <section class="settings-section">
    <h3>Behavior</h3>

    <div class="setting-row">
      <div class="setting-info">
        <span class="setting-label">Confirm before ending task</span>
        <span class="setting-desc">Show confirmation when killing processes</span>
      </div>
      <label class="toggle">
        <input
          type="checkbox"
          checked={settings.confirmKill}
          onchange={() => handleToggle('confirmKill')}
        />
        <span class="toggle-track"></span>
      </label>
    </div>
  </section>

  <section class="settings-section">
    <h3>About</h3>
    <div class="about-info">
      <div class="about-row">
        <span>WSL Task Manager</span>
        <span class="about-value">v2.0.0</span>
      </div>
      <div class="about-row">
        <span>Built with</span>
        <span class="about-value">Tauri + Svelte</span>
      </div>
    </div>
  </section>
</div>

<style>
  .settings-tab {
    padding: var(--space-4);
    overflow: auto;
    max-width: 600px;
  }

  .settings-section {
    background: var(--surface-raised);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-lg);
    padding: var(--space-4);
    margin-bottom: var(--space-4);
  }

  .settings-section h3 {
    margin-bottom: var(--space-4);
    font-size: var(--text-sm);
    font-weight: 600;
    color: var(--text-secondary);
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .setting-row {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: var(--space-3) 0;
    border-bottom: 1px solid var(--border-subtle);
  }

  .setting-row:last-child {
    border-bottom: none;
  }

  .setting-info {
    display: flex;
    flex-direction: column;
    gap: var(--space-1);
  }

  .setting-label {
    font-size: var(--text-base);
    font-weight: 500;
    color: var(--text-primary);
  }

  .setting-desc {
    font-size: var(--text-sm);
    color: var(--text-muted);
  }

  select {
    padding: var(--space-2) var(--space-3);
    background: var(--surface-overlay);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-md);
    color: var(--text-primary);
    font-size: var(--text-sm);
    cursor: pointer;
  }

  select:hover {
    border-color: var(--border-default);
  }

  /* Toggle switch */
  .toggle {
    position: relative;
    display: inline-block;
    width: 44px;
    height: 24px;
  }

  .toggle input {
    opacity: 0;
    width: 0;
    height: 0;
  }

  .toggle-track {
    position: absolute;
    inset: 0;
    background: var(--surface-base);
    border: 1px solid var(--border-default);
    border-radius: 12px;
    cursor: pointer;
    transition: all var(--transition-base);
  }

  .toggle-track::before {
    content: '';
    position: absolute;
    height: 18px;
    width: 18px;
    left: 2px;
    bottom: 2px;
    background: var(--text-secondary);
    border-radius: 50%;
    transition: all var(--transition-base);
  }

  .toggle input:checked + .toggle-track {
    background: var(--accent);
    border-color: var(--accent);
  }

  .toggle input:checked + .toggle-track::before {
    transform: translateX(20px);
    background: white;
  }

  .about-info {
    background: var(--surface-base);
    border-radius: var(--radius-md);
    padding: var(--space-1) 0;
  }

  .about-row {
    display: flex;
    justify-content: space-between;
    padding: var(--space-3) var(--space-4);
    font-size: var(--text-sm);
  }

  .about-row span:first-child {
    color: var(--text-secondary);
  }

  .about-value {
    color: var(--text-primary);
    font-weight: 500;
  }
</style>
