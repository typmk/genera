// ============================================
// Type Definitions
// ============================================

export interface Process {
  pid: number;
  name: string;
  shortName: string;
  command: string;
  state: string;
  user: string;
  cpuPercent: number;
  memoryMb: number;
  threads: number;
  parentPid: number | null;
  source: 'windows' | string; // 'windows' | 'wsl:distro-name'
}

export interface SystemStats {
  cpuPercent: number;
  loadAvg: [number, number, number];
  memory: {
    totalMb: number;
    usedMb: number;
    availableMb: number;
    percent: number;
  };
  uptime: number;
  hostname: string;
  kernel: string;
}

export interface CoreUsage {
  index: number;
  usage: number;
  topProcess?: string;
}

export interface GpuInfo {
  name: string;
  utilizationPercent: number;
  memoryUsedMb: number;
  memoryTotalMb: number;
  temperature?: number;
}

export interface NetworkConnection {
  id: string;
  localAddr: string;
  localPort: number;
  remoteAddr: string;
  remotePort: number;
  state: string;
  protocol: 'tcp' | 'udp';
  pid: number;
  processName: string;
  source: string;
}

export interface Service {
  name: string;
  displayName: string;
  status: 'running' | 'stopped' | 'unknown';
  startType: string;
  source: 'windows' | 'linux';
}

export interface StartupApp {
  name: string;
  publisher: string;
  status: 'enabled' | 'disabled';
  startupType: string;
  location: string;
}

export interface SystemSnapshot {
  processes: Process[];
  system: SystemStats;
  timestamp: number;
}

// Tree node for hierarchical process view
export interface ProcessNode extends Process {
  children: ProcessNode[];
  expanded: boolean;
  depth: number;
}

// Column definition for tables
export interface Column<T = any> {
  key: keyof T | string;
  label: string;
  width?: string;
  sortable?: boolean;
  align?: 'left' | 'center' | 'right';
  render?: (value: any, row: T) => string;
}

// Context menu item
export interface MenuItem {
  label?: string;
  icon?: string;
  action?: () => void;
  danger?: boolean;
  disabled?: boolean;
  type?: 'divider';
  children?: MenuItem[];
}

// Sort state
export interface SortState {
  key: string;
  direction: 'asc' | 'desc';
}

// Tab definition
export interface Tab {
  id: string;
  label: string;
  icon: string;
  component: () => Promise<any>;
}
