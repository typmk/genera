<script lang="ts">
  import { onMount } from 'svelte';
  import { tabs, getTab, loadSettings, handleKeydown } from '$lib/stores/ui.svelte';
  import { startPolling, stopPolling } from '$lib/tauri';
  import Titlebar from '$lib/components/Titlebar.svelte';
  import Sidebar from '$lib/components/Sidebar.svelte';

  // Pre-fetch network connections in background
  import '$lib/stores/network.svelte';

  // Eagerly import all tabs for fast switching
  import Processes from '$lib/tabs/Processes.svelte';
  import Details from '$lib/tabs/Details.svelte';
  import Performance from '$lib/tabs/Performance.svelte';
  import Network from '$lib/tabs/Network.svelte';
  import Explore from '$lib/tabs/Explore.svelte';
  import Settings from '$lib/tabs/Settings.svelte';

  const tabComponents: Record<string, any> = {
    processes: Processes,
    details: Details,
    performance: Performance,
    network: Network,
    explore: Explore,
    settings: Settings,
  };

  let currentTabId = $derived(getTab());
  let TabComponent = $derived(tabComponents[currentTabId] || Processes);

  onMount(() => {
    loadSettings();
    startPolling();

    // Global keyboard shortcuts
    window.addEventListener('keydown', handleKeydown);

    return () => {
      stopPolling();
      window.removeEventListener('keydown', handleKeydown);
    };
  });
</script>

<svelte:head>
  <title>WSL Task Manager</title>
</svelte:head>

<div class="app">
  <Titlebar />

  <div class="app-container">
    <Sidebar />

    <main class="main-content">
      <TabComponent />
    </main>
  </div>
</div>

<style>
  .app {
    display: flex;
    flex-direction: column;
    height: 100vh;
    overflow: hidden;
    background: var(--surface-base);
  }

  .app-container {
    flex: 1;
    display: flex;
    overflow: hidden;
  }

  .main-content {
    flex: 1;
    display: flex;
    flex-direction: column;
    overflow: hidden;
    background: var(--surface-base);
  }
</style>
