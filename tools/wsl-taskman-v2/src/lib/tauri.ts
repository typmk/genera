// ============================================
// Tauri Bridge - Backend communication
// ============================================

import { invoke as tauriInvoke } from '@tauri-apps/api/core';
import type { Process, SystemStats, CoreUsage, GpuInfo, NetworkConnection, Service, StartupApp } from './types';
import { setProcesses } from './stores/processes.svelte';
import { setStats, setCores, setGpu, setSources, getActiveSource } from './stores/system.svelte';
import { getSettings } from './stores/ui.svelte';

// ============================================
// Type-safe invoke wrappers
// ============================================

interface SystemSnapshot {
  processes: Process[];
  stats: SystemStats;
  cores: CoreUsage[];
  gpu: GpuInfo | null;
}

export async function fetchSnapshot(): Promise<void> {
  try {
    const source = getActiveSource();
    const data = await tauriInvoke<SystemSnapshot>('get_system_snapshot', {
      source: source === 'all' ? null : source
    });

    // Update all stores atomically
    setProcesses(data.processes);
    setStats(data.stats);
    setCores(data.cores);
    setGpu(data.gpu);
  } catch (error) {
    console.error('Failed to fetch snapshot:', error);
  }
}

export async function fetchSources(): Promise<void> {
  try {
    const sources = await tauriInvoke<string[]>('get_sources');
    setSources(sources);
  } catch (error) {
    console.error('Failed to fetch sources:', error);
  }
}

// ============================================
// Process actions
// ============================================

export async function killProcess(pid: number, tree = false): Promise<boolean> {
  try {
    await tauriInvoke('kill_process', { pid, tree });
    return true;
  } catch (error) {
    console.error('Failed to kill process:', error);
    return false;
  }
}

export async function setPriority(pid: number, priority: string): Promise<boolean> {
  try {
    await tauriInvoke('set_priority', { pid, priority });
    return true;
  } catch (error) {
    console.error('Failed to set priority:', error);
    return false;
  }
}

export async function setAffinity(pid: number, mask: number): Promise<boolean> {
  try {
    await tauriInvoke('set_affinity', { pid, mask });
    return true;
  } catch (error) {
    console.error('Failed to set affinity:', error);
    return false;
  }
}

// ============================================
// Network
// ============================================

export async function getNetworkConnections(): Promise<NetworkConnection[]> {
  try {
    return await tauriInvoke<NetworkConnection[]>('get_network_connections');
  } catch (error) {
    console.error('Failed to fetch network connections:', error);
    return [];
  }
}

export async function whoisLookup(ip: string): Promise<string> {
  try {
    return await tauriInvoke<string>('whois_lookup', { ip });
  } catch (error) {
    console.error('Whois lookup failed:', error);
    return `Error: ${error}`;
  }
}

// ============================================
// Services
// ============================================

export async function getServices(source: 'windows' | 'linux'): Promise<Service[]> {
  try {
    return await tauriInvoke<Service[]>('get_services', { source });
  } catch (error) {
    console.error('Failed to fetch services:', error);
    return [];
  }
}

export async function controlService(name: string, action: 'start' | 'stop' | 'restart', source: 'windows' | 'linux'): Promise<boolean> {
  try {
    await tauriInvoke('control_service', { name, action, source });
    return true;
  } catch (error) {
    console.error('Failed to control service:', error);
    return false;
  }
}

// ============================================
// Startup apps
// ============================================

export async function getStartupApps(): Promise<StartupApp[]> {
  try {
    return await tauriInvoke<StartupApp[]>('get_startup_apps');
  } catch (error) {
    console.error('Failed to fetch startup apps:', error);
    return [];
  }
}

export async function toggleStartupApp(name: string, enabled: boolean): Promise<boolean> {
  try {
    await tauriInvoke('toggle_startup_app', { name, enabled });
    return true;
  } catch (error) {
    console.error('Failed to toggle startup app:', error);
    return false;
  }
}

// ============================================
// Tools
// ============================================

export async function launchTool(tool: string, args: string[] = []): Promise<void> {
  try {
    await tauriInvoke('launch_tool', { tool, args });
  } catch (error) {
    console.error('Failed to launch tool:', error);
  }
}

// ============================================
// Polling
// ============================================

let pollInterval: number | null = null;

export function startPolling() {
  if (pollInterval) return;

  const poll = async () => {
    await fetchSnapshot();
    const { refreshRate } = getSettings();
    pollInterval = window.setTimeout(poll, refreshRate);
  };

  // Initial fetch
  fetchSources();
  poll();
}

export function stopPolling() {
  if (pollInterval) {
    clearTimeout(pollInterval);
    pollInterval = null;
  }
}

export function restartPolling() {
  stopPolling();
  startPolling();
}
