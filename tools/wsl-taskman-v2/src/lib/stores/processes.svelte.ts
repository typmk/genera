// ============================================
// Process Store - Single source of truth
// ============================================

import type { Process, ProcessNode, SortState } from '$lib/types';

// Raw process data (normalized map for O(1) lookups)
let processMap = $state<Map<number, Process>>(new Map());

// Sort state
let sort = $state<SortState>({ key: 'cpuPercent', direction: 'desc' });

// Search filter
let search = $state('');

// Expanded nodes in tree view
let expanded = $state<Set<number>>(new Set([0])); // 0 = root

// ============================================
// Setters
// ============================================

export function setProcesses(processes: Process[]) {
  processMap = new Map(processes.map(p => [p.pid, p]));
}

export function setSort(key: string, direction?: 'asc' | 'desc') {
  if (sort.key === key && !direction) {
    sort.direction = sort.direction === 'asc' ? 'desc' : 'asc';
  } else {
    sort = { key, direction: direction ?? 'desc' };
  }
}

export function setSearch(query: string) {
  search = query;
}

export function toggleExpanded(pid: number) {
  if (expanded.has(pid)) {
    expanded.delete(pid);
  } else {
    expanded.add(pid);
  }
  expanded = new Set(expanded); // Trigger reactivity
}

// ============================================
// Derived state (computed)
// ============================================

// All processes as array
export function getProcesses(): Process[] {
  return [...processMap.values()];
}

// Filtered and sorted list
export function getProcessList(): Process[] {
  let list = [...processMap.values()];

  // Filter by search
  if (search) {
    const q = search.toLowerCase();
    list = list.filter(p =>
      p.name.toLowerCase().includes(q) ||
      p.shortName.toLowerCase().includes(q) ||
      p.pid.toString().includes(q)
    );
  }

  // Sort
  const key = sort.key as keyof Process;
  const dir = sort.direction === 'asc' ? 1 : -1;

  list.sort((a, b) => {
    const aVal = a[key];
    const bVal = b[key];

    if (typeof aVal === 'number' && typeof bVal === 'number') {
      return (aVal - bVal) * dir;
    }
    return String(aVal).localeCompare(String(bVal)) * dir;
  });

  return list;
}

// Process tree for hierarchical view
export function getProcessTree(): ProcessNode[] {
  const processes = [...processMap.values()];
  const byParent = new Map<number | null, Process[]>();

  // Group by parent
  for (const p of processes) {
    const parent = p.parentPid;
    if (!byParent.has(parent)) {
      byParent.set(parent, []);
    }
    byParent.get(parent)!.push(p);
  }

  // Build tree recursively
  function buildNode(p: Process, depth: number): ProcessNode {
    const children = (byParent.get(p.pid) || [])
      .map(child => buildNode(child, depth + 1))
      .sort((a, b) => b.cpuPercent - a.cpuPercent);

    return {
      ...p,
      children,
      expanded: expanded.has(p.pid),
      depth
    };
  }

  // Start from root processes (no parent or parent not in our list)
  const roots = processes.filter(p =>
    p.parentPid === null || !processMap.has(p.parentPid)
  );

  return roots
    .map(p => buildNode(p, 0))
    .sort((a, b) => b.cpuPercent - a.cpuPercent);
}

// Grouped by user
export function getByUser(): Map<string, Process[]> {
  const grouped = new Map<string, Process[]>();

  for (const p of processMap.values()) {
    const user = p.user || 'Unknown';
    if (!grouped.has(user)) {
      grouped.set(user, []);
    }
    grouped.get(user)!.push(p);
  }

  return grouped;
}

// Top memory consumers
export function getTopMemory(limit = 10): Process[] {
  return [...processMap.values()]
    .sort((a, b) => b.memoryMb - a.memoryMb)
    .slice(0, limit);
}

// Top CPU consumers
export function getTopCpu(limit = 10): Process[] {
  return [...processMap.values()]
    .sort((a, b) => b.cpuPercent - a.cpuPercent)
    .slice(0, limit);
}

// Get single process by PID
export function getProcess(pid: number): Process | undefined {
  return processMap.get(pid);
}

// Get current sort state
export function getSort(): SortState {
  return sort;
}

// Get current search query
export function getSearch(): string {
  return search;
}
