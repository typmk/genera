// ============================================
// UI Store - Navigation, selection, preferences
// ============================================

import type { Tab } from '$lib/types';

// Available tabs
export const tabs: Tab[] = [
  { id: 'processes', label: 'Processes', icon: 'list-tree', component: () => import('$lib/tabs/Processes.svelte') },
  { id: 'details', label: 'Details', icon: 'table', component: () => import('$lib/tabs/Details.svelte') },
  { id: 'performance', label: 'Performance', icon: 'chart', component: () => import('$lib/tabs/Performance.svelte') },
  { id: 'network', label: 'Network', icon: 'network', component: () => import('$lib/tabs/Network.svelte') },
  { id: 'explore', label: 'Explore', icon: 'microscope', component: () => import('$lib/tabs/Explore.svelte') },
  { id: 'settings', label: 'Settings', icon: 'settings', component: () => import('$lib/tabs/Settings.svelte') },
];

// Current tab
let currentTab = $state('processes');

// Selected process PID(s)
let selectedPids = $state<Set<number>>(new Set());

// Context menu state
let contextMenu = $state<{
  show: boolean;
  x: number;
  y: number;
  target: any;
}>({ show: false, x: 0, y: 0, target: null });

// Settings
let settings = $state({
  refreshRate: 2000,
  compactMode: false,
  showSystemProcesses: true,
  confirmKill: true,
});

// ============================================
// Tab navigation
// ============================================

export function setTab(tabId: string) {
  currentTab = tabId;
}

export function getTab(): string {
  return currentTab;
}

export function getTabConfig(): Tab | undefined {
  return tabs.find(t => t.id === currentTab);
}

// ============================================
// Selection
// ============================================

export function selectPid(pid: number, multi = false) {
  if (multi) {
    if (selectedPids.has(pid)) {
      selectedPids.delete(pid);
    } else {
      selectedPids.add(pid);
    }
    selectedPids = new Set(selectedPids);
  } else {
    selectedPids = new Set([pid]);
  }
}

export function clearSelection() {
  selectedPids = new Set();
}

export function getSelectedPids(): Set<number> {
  return selectedPids;
}

export function isSelected(pid: number): boolean {
  return selectedPids.has(pid);
}

export function getFirstSelectedPid(): number | null {
  const first = selectedPids.values().next();
  return first.done ? null : first.value;
}

// ============================================
// Context Menu
// ============================================

export function showContextMenu(x: number, y: number, target: any) {
  contextMenu = { show: true, x, y, target };
}

export function hideContextMenu() {
  contextMenu = { ...contextMenu, show: false };
}

export function getContextMenu() {
  return contextMenu;
}

// ============================================
// Settings
// ============================================

export function getSettings() {
  return settings;
}

export function updateSettings(updates: Partial<typeof settings>) {
  settings = { ...settings, ...updates };
  saveSettings();
}

function saveSettings() {
  try {
    localStorage.setItem('wsl-taskman-settings', JSON.stringify(settings));
  } catch (e) {
    // Ignore
  }
}

export function loadSettings() {
  try {
    const saved = localStorage.getItem('wsl-taskman-settings');
    if (saved) {
      settings = { ...settings, ...JSON.parse(saved) };
    }
  } catch (e) {
    // Ignore
  }
}

// ============================================
// Keyboard shortcuts
// ============================================

const shortcuts = new Map<string, () => void>();

export function registerShortcut(key: string, handler: () => void) {
  shortcuts.set(key, handler);
}

export function handleKeydown(event: KeyboardEvent) {
  const key = [
    event.ctrlKey && 'ctrl',
    event.shiftKey && 'shift',
    event.altKey && 'alt',
    event.key.toLowerCase()
  ].filter(Boolean).join('+');

  const handler = shortcuts.get(key);
  if (handler) {
    event.preventDefault();
    handler();
  }
}
