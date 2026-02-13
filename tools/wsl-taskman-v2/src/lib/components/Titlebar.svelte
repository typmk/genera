<script lang="ts">
  import { getCurrentWindow } from '@tauri-apps/api/window';
  import { setSearch, getSearch } from '$lib/stores/processes.svelte';

  let searchValue = $state(getSearch());
  let searchTimeout: ReturnType<typeof setTimeout> | null = null;

  function handleSearch(event: Event) {
    const input = event.target as HTMLInputElement;
    searchValue = input.value;

    // Debounce the actual filter update
    if (searchTimeout) clearTimeout(searchTimeout);
    searchTimeout = setTimeout(() => {
      setSearch(searchValue);
    }, 150);
  }

  function handleMinimize() {
    getCurrentWindow().minimize();
  }

  function handleMaximize() {
    getCurrentWindow().toggleMaximize();
  }

  function handleClose() {
    // Hide to tray for instant re-show (<5ms)
    getCurrentWindow().hide();
  }

  function handleDrag(event: MouseEvent) {
    // Only start drag on left click and if not on a button/input
    if (event.button === 0) {
      getCurrentWindow().startDragging();
    }
  }

  function handleDoubleClick() {
    getCurrentWindow().toggleMaximize();
  }
</script>

<!-- svelte-ignore a11y_no_noninteractive_element_interactions a11y_no_static_element_interactions -->
<header
  class="titlebar"
  role="toolbar"
  onmousedown={handleDrag}
  ondblclick={handleDoubleClick}>
  <div class="titlebar-left">
    <span class="app-icon">⚡</span>
    <span class="app-title">WSL Task Manager</span>
  </div>

  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="titlebar-center" onmousedown={(e) => e.stopPropagation()}>
    <div class="search-box">
      <span class="search-icon">🔍</span>
      <input
        type="text"
        placeholder="Search processes..."
        value={searchValue}
        oninput={handleSearch}
        autocomplete="off"
        spellcheck="false"
      />
    </div>
  </div>

  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div class="titlebar-right" onmousedown={(e) => e.stopPropagation()}>
    <button class="window-btn" onclick={handleMinimize} title="Minimize">
      <span>─</span>
    </button>
    <button class="window-btn" onclick={handleMaximize} title="Maximize">
      <span>☐</span>
    </button>
    <button class="window-btn close" onclick={handleClose} title="Close">
      <span>✕</span>
    </button>
  </div>
</header>

<style>
  .titlebar {
    height: var(--titlebar-height);
    display: flex;
    align-items: center;
    padding: 0 var(--space-4);
    background: var(--surface-raised);
    border-bottom: 1px solid var(--border-subtle);
    cursor: grab;
    user-select: none;
  }

  .titlebar:active {
    cursor: grabbing;
  }

  .titlebar-left {
    display: flex;
    align-items: center;
    gap: var(--space-3);
  }

  .app-icon {
    font-size: 18px;
  }

  .app-title {
    font-size: var(--text-base);
    font-weight: 600;
    color: var(--text-primary);
  }

  .titlebar-center {
    flex: 1;
    display: flex;
    justify-content: center;
    padding: 0 var(--space-8);
    cursor: default;
  }

  .search-box {
    display: flex;
    align-items: center;
    gap: var(--space-2);
    width: 100%;
    max-width: 400px;
    padding: var(--space-2) var(--space-3);
    background: var(--surface-base);
    border: 1px solid var(--border-subtle);
    border-radius: var(--radius-lg);
    transition: border-color var(--transition-fast);
  }

  .search-box:focus-within {
    border-color: var(--accent);
  }

  .search-icon {
    font-size: 12px;
    opacity: 0.5;
  }

  .search-box input {
    flex: 1;
    border: none;
    background: transparent;
    color: var(--text-primary);
    font-size: var(--text-sm);
    outline: none;
  }

  .search-box input::placeholder {
    color: var(--text-muted);
  }

  .titlebar-right {
    display: flex;
    gap: var(--space-1);
    cursor: default;
  }

  .window-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 36px;
    height: 28px;
    border: none;
    background: transparent;
    color: var(--text-secondary);
    font-size: 12px;
    cursor: pointer;
    border-radius: var(--radius-sm);
    transition: all var(--transition-fast);
  }

  .window-btn:hover {
    background: var(--surface-hover);
    color: var(--text-primary);
  }

  .window-btn.close:hover {
    background: var(--color-danger);
    color: white;
  }
</style>
