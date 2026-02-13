// ============================================
// System Store - CPU, Memory, GPU stats
// ============================================

import type { SystemStats, CoreUsage, GpuInfo } from '$lib/types';

// System stats
let stats = $state<SystemStats | null>(null);

// Per-core CPU usage
let cores = $state<CoreUsage[]>([]);

// GPU info
let gpu = $state<GpuInfo | null>(null);

// History for charts (last 60 samples)
let cpuHistory = $state<number[]>([]);
let memHistory = $state<number[]>([]);
const MAX_HISTORY = 60;

// Available sources (Windows, WSL distros)
let sources = $state<string[]>(['windows']);
let activeSource = $state('all');

// ============================================
// Setters
// ============================================

export function setStats(newStats: SystemStats) {
  stats = newStats;

  // Update history
  cpuHistory = [...cpuHistory.slice(-(MAX_HISTORY - 1)), newStats.cpuPercent];
  memHistory = [...memHistory.slice(-(MAX_HISTORY - 1)), newStats.memory.percent];
}

export function setCores(newCores: CoreUsage[]) {
  cores = newCores;
}

export function setGpu(newGpu: GpuInfo | null) {
  gpu = newGpu;
}

export function setSources(newSources: string[]) {
  sources = newSources;
}

export function setActiveSource(source: string) {
  activeSource = source;
  // Clear history on source change
  cpuHistory = [];
  memHistory = [];
}

// ============================================
// Getters
// ============================================

export function getStats(): SystemStats | null {
  return stats;
}

export function getCores(): CoreUsage[] {
  return cores;
}

export function getGpu(): GpuInfo | null {
  return gpu;
}

export function getCpuHistory(): number[] {
  return cpuHistory;
}

export function getMemHistory(): number[] {
  return memHistory;
}

export function getSources(): string[] {
  return sources;
}

export function getActiveSource(): string {
  return activeSource;
}

// ============================================
// Computed helpers
// ============================================

export function getCpuPercent(): number {
  return stats?.cpuPercent ?? 0;
}

export function getMemPercent(): number {
  return stats?.memory.percent ?? 0;
}

export function getMemUsed(): string {
  if (!stats) return '-- / --';
  const used = (stats.memory.usedMb / 1024).toFixed(1);
  const total = (stats.memory.totalMb / 1024).toFixed(1);
  return `${used} / ${total} GB`;
}

export function getUptime(): string {
  if (!stats) return '--';
  const secs = stats.uptime;
  const days = Math.floor(secs / 86400);
  const hours = Math.floor((secs % 86400) / 3600);
  const mins = Math.floor((secs % 3600) / 60);

  if (days > 0) return `${days}d ${hours}h`;
  if (hours > 0) return `${hours}h ${mins}m`;
  return `${mins}m`;
}
