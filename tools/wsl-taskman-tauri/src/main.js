const { invoke } = window.__TAURI__.core;
const { listen } = window.__TAURI__.event;
const { getCurrentWindow } = window.__TAURI__.window;

// Initialized in DOMContentLoaded
let appWindow;

// Restore cached session data for instant render
function restoreCachedSession() {
  try {
    const cached = localStorage.getItem('wsl-taskman-session');
    if (cached) {
      const data = JSON.parse(cached);
      if (data.processes) cachedProcesses = data.processes;
      if (data.stats) cachedStats = data.stats;
      if (data.systemTree) cachedSystemTree = data.systemTree;
      // Restore all tab caches for instant switching
      if (data.windowsServices) cachedWindowsServices = data.windowsServices;
      if (data.linuxServices) cachedLinuxServices = data.linuxServices;
      if (data.networkConnections) cachedNetworkConnections = data.networkConnections;
      if (data.startupApps) cachedStartupApps = data.startupApps;
      return true;
    }
  } catch (e) { /* ignore */ }
  return false;
}

// Save session for next startup
function saveSessionCache() {
  try {
    const data = {
      processes: cachedProcesses.slice(0, 100), // Limit size
      stats: cachedStats,
      systemTree: cachedSystemTree,
      // Cache all tabs for instant restore
      windowsServices: cachedWindowsServices.slice(0, 50),
      linuxServices: cachedLinuxServices.slice(0, 50),
      networkConnections: cachedNetworkConnections.slice(0, 100),
      startupApps: cachedStartupApps.slice(0, 30),
      timestamp: Date.now()
    };
    localStorage.setItem('wsl-taskman-session', JSON.stringify(data));
  } catch (e) { /* ignore */ }
}

// Defer non-critical work
function deferWork(fn) {
  if ('requestIdleCallback' in window) {
    requestIdleCallback(fn, { timeout: 1000 });
  } else {
    setTimeout(fn, 50);
  }
}

// State
let processes = [];
let allProcesses = []; // All processes from all sources for local filtering
let cachedProcesses = []; // Current view's filtered processes
let cachedStats = null;   // Cached stats for instant UI
let cachedStatsBySource = new Map(); // Cache per source for instant switching
let selectedPid = null;
let sortBy = 'memory_mb';
let sortAsc = false;
let searchQuery = '';
let expandedPids = new Set();
let currentTab = 'processes';
let currentSource = 'all'; // 'all', 'windows', 'wsl:distro-name'
let availableSources = [];
const cpuHistory = [];
const memHistory = [];
const MAX_HISTORY = 60;
let isRefreshing = false; // Prevent concurrent refreshes
let refreshInterval = null; // Store interval ID for refresh rate changes

// Settings with defaults
const defaultSettings = {
  refreshRate: 2000,
  defaultTab: 'processes',
  showSystem: true,
  compactMode: false,
  confirmKill: true,
  startMinimized: false,
  autostart: false
};

let settings = { ...defaultSettings };

// Load settings from localStorage
function loadSettings() {
  try {
    const saved = localStorage.getItem('wsl-taskman-settings');
    if (saved) {
      settings = { ...defaultSettings, ...JSON.parse(saved) };
    }
  } catch (e) {
    console.error('Failed to load settings:', e);
  }
  applySettings();
}

// Save settings to localStorage
function saveSettings() {
  try {
    localStorage.setItem('wsl-taskman-settings', JSON.stringify(settings));
  } catch (e) {
    console.error('Failed to save settings:', e);
  }
}

// Apply settings to UI
function applySettings() {
  // Compact mode
  if (settings.compactMode) {
    document.body.classList.add('compact-mode');
  } else {
    document.body.classList.remove('compact-mode');
  }

  // Update form elements to match settings
  const refreshRateEl = document.getElementById('setting-refresh-rate');
  const defaultTabEl = document.getElementById('setting-default-tab');
  const showSystemEl = document.getElementById('setting-show-system');
  const compactModeEl = document.getElementById('setting-compact-mode');
  const confirmKillEl = document.getElementById('setting-confirm-kill');
  const startMinimizedEl = document.getElementById('setting-start-minimized');
  const autostartEl = document.getElementById('setting-autostart');

  if (refreshRateEl) refreshRateEl.value = settings.refreshRate;
  if (defaultTabEl) defaultTabEl.value = settings.defaultTab;
  if (showSystemEl) showSystemEl.checked = settings.showSystem;
  if (compactModeEl) compactModeEl.checked = settings.compactMode;
  if (confirmKillEl) confirmKillEl.checked = settings.confirmKill;
  if (startMinimizedEl) startMinimizedEl.checked = settings.startMinimized;
  if (autostartEl) autostartEl.checked = settings.autostart;
}

// Update refresh interval
function updateRefreshInterval() {
  if (refreshInterval) {
    clearInterval(refreshInterval);
  }
  refreshInterval = setInterval(refresh, settings.refreshRate);
}

// DOM Elements
const memStat = document.getElementById('mem-stat');
const memPct = document.getElementById('mem-pct');
const memDetail = document.getElementById('mem-detail');
const cpuPct = document.getElementById('cpu-pct');
const loadAvg = document.getElementById('load-avg');
const procCount = document.getElementById('proc-count');
const processTable = document.getElementById('process-table');
const detailsTable = document.getElementById('details-table');
const searchInput = document.getElementById('search');
const cpuLine = document.getElementById('cpu-line');
const memLine = document.getElementById('mem-line');
const sidebarCpu = document.getElementById('sidebar-cpu');
const sidebarMem = document.getElementById('sidebar-mem');
const killBtn = document.getElementById('kill-btn');
const contextMenu = document.getElementById('context-menu');
const sourceSelect = document.getElementById('source-select');
const systemTree = document.getElementById('system-tree');
const usersTable = document.getElementById('users-table');
const servicesTable = document.getElementById('services-table');
const networkTable = document.getElementById('network-table');
const startupTable = document.getElementById('startup-table');
const coreBars = document.getElementById('core-bars');

// Tree expansion state
const expandedNodes = new Set(['windows', 'vmmem']); // Start with host and VM container expanded

// Utility
const getMemClass = (pct) => pct > 85 ? 'red' : pct > 70 ? 'orange' : 'green';
const getCpuClass = (pct) => pct > 50 ? 'cpu-high' : pct > 20 ? 'cpu-med' : '';
const getMemCellClass = (mb) => mb > 500 ? 'mem-high' : mb > 200 ? 'mem-med' : '';

// Format uptime
function formatUptime(secs) {
  const days = Math.floor(secs / 86400);
  const hours = Math.floor((secs % 86400) / 3600);
  const mins = Math.floor((secs % 3600) / 60);
  if (days > 0) return `${days}d ${hours}h ${mins}m`;
  if (hours > 0) return `${hours}h ${mins}m`;
  return `${mins}m`;
}

// Smart command truncation - show start and important end parts
function truncateCommand(cmd, maxLen = 60) {
  if (cmd.length <= maxLen) return cmd;
  const start = cmd.substring(0, Math.floor(maxLen * 0.6));
  const end = cmd.substring(cmd.length - Math.floor(maxLen * 0.3));
  return `${start}...${end}`;
}

// Debounce
function debounce(fn, ms) {
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), ms);
  };
}

// Throttle
function throttle(fn, ms) {
  let lastCall = 0;
  let pending = false;
  return (...args) => {
    const now = Date.now();
    if (now - lastCall >= ms) {
      lastCall = now;
      fn(...args);
    } else if (!pending) {
      pending = true;
      setTimeout(() => {
        pending = false;
        lastCall = Date.now();
        fn(...args);
      }, ms - (now - lastCall));
    }
  };
}

// Safe DOM element creation
function createEl(tag, attrs = {}, textContent = '') {
  const el = document.createElement(tag);
  Object.entries(attrs).forEach(([k, v]) => {
    if (k === 'className') el.className = v;
    else if (k === 'style') Object.assign(el.style, v);
    else el.setAttribute(k, v);
  });
  if (textContent) el.textContent = textContent;
  return el;
}

// Build process tree structure
function buildProcessTree(procs) {
  const map = new Map();
  const roots = [];

  procs.forEach(p => {
    map.set(p.pid, { ...p, children: [] });
  });

  procs.forEach(p => {
    const node = map.get(p.pid);
    const parent = map.get(p.ppid);
    if (parent && p.ppid !== p.pid) {
      parent.children.push(node);
    } else {
      roots.push(node);
    }
  });

  return roots;
}

// Flatten tree with depth info
function flattenTree(nodes, depth = 0) {
  const result = [];
  nodes.forEach(node => {
    result.push({ ...node, depth, hasChildren: node.children.length > 0 });
    if (expandedPids.has(node.pid) && node.children.length > 0) {
      result.push(...flattenTree(node.children, depth + 1));
    }
  });
  return result;
}

// Group processes like Windows Task Manager
function groupProcesses(procs) {
  const apps = procs.filter(p => !p.is_system && p.cpu_percent > 0.1);
  const background = procs.filter(p => p.is_system || p.cpu_percent <= 0.1);
  return { apps, background };
}

// Update stats display
function updateStats(stats, sourceId = currentSource) {
  const { memory, cpu_percent, load_avg } = stats;
  processes = stats.processes || [];

  // Cache for instant rendering
  cachedProcesses = processes;
  cachedStats = stats;
  cachedStatsBySource.set(sourceId, stats);

  // Also update allProcesses with all known processes (for local source filtering)
  if (sourceId === 'all') {
    allProcesses = [...processes];
  } else {
    // Merge into allProcesses (update existing, add new)
    const existingPids = new Set(allProcesses.map(p => `${p.source}:${p.pid}`));
    processes.forEach(p => {
      const key = `${p.source}:${p.pid}`;
      if (!existingPids.has(key)) {
        allProcesses.push(p);
      }
    });
  }

  // Header stats
  if (memPct) memPct.textContent = `${memory.percent.toFixed(0)}%`;
  if (memDetail) memDetail.textContent = `${memory.used_mb.toLocaleString()} / ${memory.total_mb.toLocaleString()} MiB`;
  if (memStat) memStat.className = `stat-chip ${getMemClass(memory.percent)}`;
  if (cpuPct) cpuPct.textContent = `${cpu_percent.toFixed(1)}%`;
  if (loadAvg) loadAvg.textContent = load_avg[0].toFixed(2);
  if (procCount) procCount.textContent = processes.length;

  // Sidebar quick stats
  if (sidebarCpu) sidebarCpu.textContent = `${cpu_percent.toFixed(1)}%`;
  if (sidebarMem) sidebarMem.textContent = `${memory.percent.toFixed(0)}%`;

  // History for charts
  cpuHistory.push(cpu_percent);
  memHistory.push(memory.percent);
  if (cpuHistory.length > MAX_HISTORY) cpuHistory.shift();
  if (memHistory.length > MAX_HISTORY) memHistory.shift();

  // Update tray tooltip
  invoke('update_tray_tooltip', {
    cpuPercent: cpu_percent,
    memoryPercent: memory.percent,
    memoryUsedMb: memory.used_mb
  }).catch(() => {});

  // Update UI - use tree view for processes panel
  if (currentTab === 'processes') {
    renderSystemTree();
  }
  renderDetailsTab();
  updateCharts();
  updatePerformancePanel(stats);
}

// Cached system tree for instant rendering
let cachedSystemTree = null;

// Filter tree nodes by search query (recursive)
function filterTreeBySearch(node, query) {
  if (!query) return node;

  // Check if this node matches
  const nameMatches = node.name.toLowerCase().includes(query);
  const pidMatches = node.pid && node.pid.toString().includes(query);
  const userMatches = node.user && node.user.toLowerCase().includes(query);
  const nodeMatches = nameMatches || pidMatches || userMatches;

  // Filter children recursively
  const filteredChildren = node.children
    .map(child => filterTreeBySearch(child, query))
    .filter(child => child !== null);

  // Include node if it matches or has matching children
  if (nodeMatches || filteredChildren.length > 0) {
    return {
      ...node,
      children: filteredChildren,
      is_expanded: true, // Auto-expand when searching
    };
  }

  return null;
}

// Render System Tree (Hierarchical view) - uses cached data for instant response
async function renderSystemTree() {
  if (!systemTree) return;

  try {
    // If we have cached data, render instantly (no backend call)
    if (cachedSystemTree) {
      const query = searchQuery.toLowerCase();
      const treeToRender = query ? filterTreeBySearch(cachedSystemTree, query) : cachedSystemTree;
      systemTree.replaceChildren();
      if (treeToRender) {
        renderTreeNode(treeToRender, systemTree, 0);
      }
      return;
    }

    // First load - get from backend
    const tree = await invoke('get_system_tree');
    cachedSystemTree = tree;
    systemTree.replaceChildren();
    renderTreeNode(tree, systemTree, 0);
  } catch (e) {
    console.error('Failed to get system tree:', e);
    // Fallback to old process rendering
    renderProcessesTabFallback();
  }
}

// Trigger background refresh and update cache when done
async function triggerBackgroundRefresh() {
  try {
    // Trigger background refresh
    await invoke('refresh_system_tree');

    // After a short delay, fetch the updated cache
    setTimeout(async () => {
      try {
        const tree = await invoke('get_system_tree');
        cachedSystemTree = tree;
        // Re-render if on processes tab
        if (currentTab === 'processes') {
          renderSystemTree();
        }
      } catch (e) {
        // Ignore - cache will update on next poll
      }
    }, 500);
  } catch (e) {
    // Silently ignore - background refresh is fire-and-forget
  }
}

// Tree sort state
let treeSortBy = 'cpu';
let treeSortAsc = false;

// Sort tree children
function sortTreeChildren(children) {
  if (!children || children.length === 0) return children;

  return [...children].sort((a, b) => {
    let cmp = 0;
    switch (treeSortBy) {
      case 'name':
        cmp = a.name.localeCompare(b.name);
        break;
      case 'cpu':
        cmp = a.cpu_percent - b.cpu_percent;
        break;
      case 'memory':
        cmp = a.memory_mb - b.memory_mb;
        break;
      case 'status':
        cmp = (a.state || '').localeCompare(b.state || '');
        break;
      default:
        cmp = a.cpu_percent - b.cpu_percent;
    }
    return treeSortAsc ? cmp : -cmp;
  });
}

// Get status text
function getStatusText(state) {
  if (!state) return '';
  const stateMap = { R: 'Running', S: 'Suspended', D: 'Disk', Z: 'Zombie', T: 'Stopped', N: 'Not responding' };
  return stateMap[state.charAt(0)] || '';
}

// Render a single tree node recursively
function renderTreeNode(node, container, depth) {
  const nodeEl = createEl('div', { className: 'tree-node', 'data-id': node.id });

  // Row with column layout
  const row = createEl('div', { className: `tree-row ${node.node_type}` });
  if (selectedPid && node.pid === selectedPid) row.classList.add('selected');

  // Name column (with indent, toggle, icon, name)
  const nameCol = createEl('div', { className: 'tree-name-col' });

  // Indentation
  for (let i = 0; i < depth; i++) {
    nameCol.appendChild(createEl('span', { className: 'tree-indent' }));
  }

  // Toggle
  const hasChildren = node.children && node.children.length > 0;
  const isExpanded = expandedNodes.has(node.id);
  const toggle = createEl('span', {
    className: `tree-toggle ${hasChildren ? (isExpanded ? 'expanded' : '') : 'empty'}`
  }, hasChildren ? '▶' : '');

  if (hasChildren) {
    toggle.addEventListener('click', (e) => {
      e.stopPropagation();
      if (expandedNodes.has(node.id)) {
        expandedNodes.delete(node.id);
      } else {
        expandedNodes.add(node.id);
      }
      renderSystemTree();
    });
  }
  nameCol.appendChild(toggle);

  // Icon
  if (node.icon) {
    nameCol.appendChild(createEl('span', { className: 'tree-icon' }, node.icon));
  }

  // Name with count
  const nameSpan = createEl('span', { className: 'tree-name' }, node.name);
  nameCol.appendChild(nameSpan);

  if (node.child_count > 0 && node.node_type !== 'process') {
    nameCol.appendChild(createEl('span', { className: 'tree-count' }, `(${node.process_count})`));
  }

  row.appendChild(nameCol);

  // CPU column
  const cpuCol = createEl('div', { className: `tree-cpu-col ${getCpuClass(node.cpu_percent)}` });
  cpuCol.textContent = node.cpu_percent > 0.1 ? `${node.cpu_percent.toFixed(1)}%` : '';
  row.appendChild(cpuCol);

  // Memory column
  const memCol = createEl('div', { className: `tree-mem-col ${getMemCellClass(node.memory_mb)}` });
  memCol.textContent = node.memory_mb > 0.1 ? `${node.memory_mb.toFixed(1)} MiB` : '';
  row.appendChild(memCol);

  // Status column
  const statusCol = createEl('div', { className: 'tree-status-col' });
  statusCol.textContent = getStatusText(node.state);
  row.appendChild(statusCol);

  // Click handler for selection (expand groups, select processes)
  row.addEventListener('click', () => {
    if (node.node_type === 'process' && node.pid) {
      selectedPid = selectedPid === node.pid ? null : node.pid;
      renderSystemTree();
      renderDetailsTab();
    } else if (hasChildren) {
      // Toggle expansion for non-process nodes
      if (expandedNodes.has(node.id)) {
        expandedNodes.delete(node.id);
      } else {
        expandedNodes.add(node.id);
      }
      renderSystemTree();
    }
  });

  // Context menu for processes
  row.addEventListener('contextmenu', (e) => {
    if (node.pid) {
      e.preventDefault();
      selectedPid = node.pid;
      renderSystemTree();
      renderDetailsTab();
      showContextMenu(e.clientX, e.clientY);
    }
  });

  nodeEl.appendChild(row);

  // Children (sorted)
  if (hasChildren && isExpanded) {
    const childrenEl = createEl('div', { className: 'tree-children expanded' });
    const sortedChildren = sortTreeChildren(node.children);
    for (const child of sortedChildren) {
      renderTreeNode(child, childrenEl, depth + 1);
    }
    nodeEl.appendChild(childrenEl);
  }

  container.appendChild(nodeEl);
}

// Fallback to old rendering if tree fails
function renderProcessesTabFallback() {
  if (!systemTree) return;

  const query = searchQuery.toLowerCase();
  let filtered = processes.filter(p =>
    !query ||
    p.short_name.toLowerCase().includes(query) ||
    p.command.toLowerCase().includes(query) ||
    p.pid.toString().includes(query)
  );

  filtered.sort((a, b) => {
    let cmp = 0;
    if (sortBy === 'short_name' || sortBy === 'state') {
      cmp = (a[sortBy] || '').localeCompare(b[sortBy] || '');
    } else {
      cmp = (a[sortBy] || 0) - (b[sortBy] || 0);
    }
    return sortAsc ? cmp : -cmp;
  });

  systemTree.replaceChildren();

  const { apps, background } = groupProcesses(filtered);

  // Simple list fallback
  const list = createEl('div', { className: 'fallback-list' });
  [...apps, ...background].forEach(p => {
    const row = createEl('div', { className: 'tree-row process' });
    row.appendChild(createEl('span', { className: 'tree-name' }, p.short_name));
    row.appendChild(createEl('span', { className: 'tree-stat' }, `${p.cpu_percent.toFixed(1)}%`));
    row.appendChild(createEl('span', { className: 'tree-stat' }, `${p.memory_mb.toFixed(1)} MiB`));
    list.appendChild(row);
  });
  systemTree.appendChild(list);

  updateKillButton();
}

// Render a single process row for Processes tab
function renderProcessRow(p, table, isTree = false) {
  const tr = createEl('tr', { 'data-pid': p.pid });
  if (selectedPid === p.pid) tr.classList.add('selected');

  // Name column with tree indent
  const nameTd = createEl('td', { className: 'col-name' });
  const nameDiv = createEl('div', { className: 'process-name' });

  if (isTree) {
    for (let i = 0; i < (p.depth || 0); i++) {
      nameDiv.appendChild(createEl('span', { className: 'tree-indent' }));
    }

    if (p.hasChildren) {
      const toggle = createEl('span', {
        className: `tree-toggle ${expandedPids.has(p.pid) ? 'expanded' : ''}`
      }, '▶');
      toggle.addEventListener('click', (e) => {
        e.stopPropagation();
        if (expandedPids.has(p.pid)) {
          expandedPids.delete(p.pid);
        } else {
          expandedPids.add(p.pid);
        }
        renderProcessesTab();
      });
      nameDiv.appendChild(toggle);
    } else if (p.depth > 0) {
      nameDiv.appendChild(createEl('span', { className: 'tree-indent' }));
    }
  }

  nameDiv.appendChild(document.createTextNode(p.short_name));
  nameTd.appendChild(nameDiv);
  tr.appendChild(nameTd);

  // CPU
  const cpuTd = createEl('td', { className: `col-cpu ${getCpuClass(p.cpu_percent)}` });
  cpuTd.textContent = p.cpu_percent > 0.1 ? `${p.cpu_percent.toFixed(1)}%` : '';
  tr.appendChild(cpuTd);

  // Memory
  const memTd = createEl('td', { className: `col-mem ${getMemCellClass(p.memory_mb)}` });
  memTd.textContent = `${p.memory_mb.toFixed(1)} MiB`;
  tr.appendChild(memTd);

  // State
  const stateMap = { R: 'Running', S: 'Suspended', D: 'Disk', Z: 'Not responding', T: 'Stopped' };
  const stateChar = (p.state || 'S').charAt(0);
  tr.appendChild(createEl('td', { className: 'col-state' }, stateMap[stateChar] || ''));

  table.appendChild(tr);
}

// Helper to get source display info
function getSourceInfo(source) {
  if (!source || source === 'windows') {
    return { icon: '🪟', label: 'Win', type: 'windows' };
  }
  if (source.startsWith('wsl:')) {
    const distro = source.replace('wsl:', '');
    return { icon: '🐧', label: distro.substring(0, 8), type: 'wsl' };
  }
  return { icon: '💻', label: source, type: 'unknown' };
}

// Render Details tab (Flat list like Windows) - uses cached data for instant response
function renderDetailsTab() {
  if (!detailsTable) return;

  // Use cached processes for instant rendering (no backend call needed)
  const dataSource = cachedProcesses.length > 0 ? cachedProcesses : processes;

  const query = searchQuery.toLowerCase();
  let filtered = dataSource.filter(p =>
    !query ||
    p.short_name.toLowerCase().includes(query) ||
    p.command.toLowerCase().includes(query) ||
    p.pid.toString().includes(query) ||
    (p.user && p.user.toLowerCase().includes(query)) ||
    (p.source && p.source.toLowerCase().includes(query))
  );

  // Sort locally - instant, no backend
  filtered.sort((a, b) => {
    let cmp = 0;
    if (sortBy === 'short_name' || sortBy === 'user' || sortBy === 'state' || sortBy === 'source') {
      cmp = (a[sortBy] || '').localeCompare(b[sortBy] || '');
    } else {
      cmp = (a[sortBy] || 0) - (b[sortBy] || 0);
    }
    return sortAsc ? cmp : -cmp;
  });

  detailsTable.replaceChildren();

  filtered.forEach(p => {
    const tr = createEl('tr', { 'data-pid': p.pid, 'data-source': p.source || '' });
    if (selectedPid === p.pid) tr.classList.add('selected');

    // Name
    tr.appendChild(createEl('td', { className: 'col-name' }, p.short_name));

    // PID
    tr.appendChild(createEl('td', { className: 'col-pid' }, p.pid.toString()));

    // Source badge
    const sourceInfo = getSourceInfo(p.source);
    const sourceTd = createEl('td', { className: 'col-source' });
    const badge = createEl('span', { className: `source-badge ${sourceInfo.type}` });
    badge.textContent = sourceInfo.label;
    sourceTd.appendChild(badge);
    tr.appendChild(sourceTd);

    // State
    const stateMap = { R: 'Running', S: 'Suspended', D: 'Disk', Z: 'Not responding', T: 'Stopped', N: 'Not responding' };
    const stateChar = (p.state || 'S').charAt(0);
    tr.appendChild(createEl('td', { className: 'col-state' }, stateMap[stateChar] || ''));

    // User
    tr.appendChild(createEl('td', { className: 'col-user' }, p.user || ''));

    // CPU
    const cpuTd = createEl('td', { className: `col-cpu ${getCpuClass(p.cpu_percent)}` });
    cpuTd.textContent = p.cpu_percent > 0.1 ? `${p.cpu_percent.toFixed(1)}%` : '';
    tr.appendChild(cpuTd);

    // Memory
    const memTd = createEl('td', { className: `col-mem ${getMemCellClass(p.memory_mb)}` });
    memTd.textContent = `${p.memory_mb.toFixed(1)} MiB`;
    tr.appendChild(memTd);

    // Command (truncated intelligently)
    const cmdTd = createEl('td', { className: 'col-command', title: p.command });
    cmdTd.textContent = truncateCommand(p.command, 50);
    tr.appendChild(cmdTd);

    detailsTable.appendChild(tr);
  });

  updateKillButton();
}

function updateKillButton() {
  if (killBtn) {
    killBtn.disabled = selectedPid === null;
  }
}

// Services sorting state
let servicesSortBy = 'name';
let servicesSortAsc = true;

// Render Users tab - aggregates processes by user
function renderUsersTab() {
  if (!usersTable) return;

  const dataSource = cachedProcesses.length > 0 ? cachedProcesses : processes;

  // Aggregate by user
  const userMap = new Map();
  dataSource.forEach(p => {
    const user = p.user || 'SYSTEM';
    const source = p.source || 'windows';
    const key = `${user}@${source}`;

    if (!userMap.has(key)) {
      userMap.set(key, {
        user,
        source,
        cpu_percent: 0,
        memory_mb: 0,
        process_count: 0,
        status: 'Active'
      });
    }

    const u = userMap.get(key);
    u.cpu_percent += p.cpu_percent || 0;
    u.memory_mb += p.memory_mb || 0;
    u.process_count += 1;
  });

  // Convert to array and sort
  let users = Array.from(userMap.values());

  // Filter by search query
  const query = searchInput?.value?.toLowerCase() || '';
  if (query) {
    users = users.filter(u => u.user?.toLowerCase().includes(query));
  }

  users.sort((a, b) => b.memory_mb - a.memory_mb);

  usersTable.replaceChildren();

  users.forEach(u => {
    const tr = createEl('tr');

    // User name
    tr.appendChild(createEl('td', { className: 'col-user-name' }, u.user));

    // Status
    const statusTd = createEl('td', { className: 'col-status' });
    const badge = createEl('span', { className: 'status-badge active' }, u.status);
    statusTd.appendChild(badge);
    tr.appendChild(statusTd);

    // CPU
    const cpuTd = createEl('td', { className: `col-cpu ${getCpuClass(u.cpu_percent)}` });
    cpuTd.textContent = u.cpu_percent > 0.1 ? `${u.cpu_percent.toFixed(1)}%` : '';
    tr.appendChild(cpuTd);

    // Memory
    const memTd = createEl('td', { className: `col-mem ${getMemCellClass(u.memory_mb)}` });
    memTd.textContent = `${u.memory_mb.toFixed(1)} MiB`;
    tr.appendChild(memTd);

    // Process count
    tr.appendChild(createEl('td', { className: 'col-procs' }, u.process_count.toString()));

    // Source
    const sourceInfo = getSourceInfo(u.source);
    const sourceTd = createEl('td', { className: 'col-source' });
    const sourceBadge = createEl('span', { className: `source-badge ${sourceInfo.type}` });
    sourceBadge.textContent = sourceInfo.label;
    sourceTd.appendChild(sourceBadge);
    tr.appendChild(sourceTd);

    usersTable.appendChild(tr);
  });
}

// Track services source
let servicesSource = 'windows';
let cachedWindowsServices = [];
let cachedLinuxServices = [];

// Network and Startup caches
let cachedNetworkConnections = [];
let cachedStartupApps = [];
let cachedCpuCores = [];
let availableDebuggers = [];

// GPU and affinity caches
let cachedGpuInfo = [];
let cachedGpuProcesses = [];
let currentAffinityPid = null;

// Render Services tab
function renderServicesTab() {
  if (!servicesTable) return;

  const services = servicesSource === 'linux' ? cachedLinuxServices : cachedWindowsServices;

  // Filter by search if active
  const query = searchInput?.value?.toLowerCase() || '';
  const filtered = query ? services.filter(s => {
    if (servicesSource === 'linux') {
      return s.unit?.toLowerCase().includes(query) || s.description?.toLowerCase().includes(query);
    } else {
      return s.name?.toLowerCase().includes(query) || s.display_name?.toLowerCase().includes(query);
    }
  }) : services;

  // Sort services
  const sorted = [...filtered].sort((a, b) => {
    let cmp = 0;
    if (servicesSource === 'linux') {
      // Linux: unit, load, active, sub, description
      const key = servicesSortBy === 'name' ? 'unit' :
                  servicesSortBy === 'display_name' ? 'description' :
                  servicesSortBy === 'status' ? 'active' :
                  servicesSortBy === 'start_type' ? 'sub' : servicesSortBy;
      cmp = (a[key] || '').localeCompare(b[key] || '');
    } else {
      // Windows
      if (servicesSortBy === 'name' || servicesSortBy === 'display_name' || servicesSortBy === 'status' || servicesSortBy === 'start_type') {
        cmp = (a[servicesSortBy] || '').localeCompare(b[servicesSortBy] || '');
      }
    }
    return servicesSortAsc ? cmp : -cmp;
  });

  servicesTable.replaceChildren();

  sorted.forEach(s => {
    const tr = createEl('tr');

    if (servicesSource === 'linux') {
      // Linux service: unit, description, active, sub (state)
      tr.appendChild(createEl('td', { className: 'col-service-name' }, s.unit));
      tr.appendChild(createEl('td', { className: 'col-display-name' }, s.description || ''));

      // Status badge (active state)
      const statusTd = createEl('td', { className: 'col-status' });
      const statusClass = s.active === 'active' ? 'running' : 'stopped';
      const badge = createEl('span', { className: `status-badge ${statusClass}` }, s.active);
      statusTd.appendChild(badge);
      tr.appendChild(statusTd);

      // Sub state (running, exited, dead, etc.)
      tr.appendChild(createEl('td', { className: 'col-start-type' }, s.sub || ''));
    } else {
      // Windows service
      tr.appendChild(createEl('td', { className: 'col-service-name' }, s.name));
      tr.appendChild(createEl('td', { className: 'col-display-name' }, s.display_name || ''));

      const statusTd = createEl('td', { className: 'col-status' });
      const statusClass = s.status === 'Running' ? 'running' : 'stopped';
      const badge = createEl('span', { className: `status-badge ${statusClass}` }, s.status);
      statusTd.appendChild(badge);
      tr.appendChild(statusTd);

      tr.appendChild(createEl('td', { className: 'col-start-type' }, s.start_type || ''));
    }

    servicesTable.appendChild(tr);
  });
}

// Load services based on current source
async function loadServices() {
  try {
    if (servicesSource === 'linux') {
      cachedLinuxServices = await invoke('get_linux_services');
    } else {
      cachedWindowsServices = await invoke('get_windows_services');
    }
    renderServicesTab();
  } catch (e) {
    console.error('Failed to load services:', e);
    if (servicesSource === 'linux') {
      cachedLinuxServices = [
        { unit: 'Error', description: 'Failed to load Linux services', active: '--', sub: '--' }
      ];
    } else {
      cachedWindowsServices = [
        { name: 'Error', display_name: 'Failed to load Windows services', status: '--', start_type: '--' }
      ];
    }
    renderServicesTab();
  }
}

// ============================================
// Network Tab
// ============================================
async function loadNetworkConnections() {
  try {
    cachedNetworkConnections = await invoke('get_network_connections');
    renderNetworkTab();
  } catch (e) {
    console.error('Failed to load network connections:', e);
    cachedNetworkConnections = [];
    renderNetworkTab();
  }
}

function renderNetworkTab() {
  if (!networkTable) return;

  const countEl = document.getElementById('network-count');
  if (countEl) countEl.textContent = `${cachedNetworkConnections.length} connections`;

  // Filter by search
  const query = searchQuery.toLowerCase();
  const filtered = query ? cachedNetworkConnections.filter(c =>
    c.local_addr?.toLowerCase().includes(query) ||
    c.remote_addr?.toLowerCase().includes(query) ||
    c.process_name?.toLowerCase().includes(query) ||
    c.state?.toLowerCase().includes(query)
  ) : cachedNetworkConnections;

  networkTable.replaceChildren();

  filtered.forEach(conn => {
    const tr = createEl('tr');
    tr.dataset.connIndex = filtered.indexOf(conn);

    // Local address
    tr.appendChild(createEl('td', { className: 'col-addr' }, conn.local_addr || '*'));

    // Local port
    tr.appendChild(createEl('td', { className: 'col-port' }, conn.local_port?.toString() || ''));

    // Remote address - clickable for whois
    const remoteAddrTd = createEl('td', { className: 'col-addr' });
    if (conn.remote_addr && conn.remote_addr !== '*' && conn.remote_addr !== '0.0.0.0') {
      const addrLink = createEl('a', { className: 'nav-link', href: '#', title: 'Whois lookup' }, conn.remote_addr);
      addrLink.addEventListener('click', (e) => {
        e.preventDefault();
        e.stopPropagation();
        whoisLookup(conn.remote_addr);
      });
      remoteAddrTd.appendChild(addrLink);
    } else {
      remoteAddrTd.textContent = conn.remote_addr || '*';
    }
    tr.appendChild(remoteAddrTd);

    // Remote port
    tr.appendChild(createEl('td', { className: 'col-port' }, conn.remote_port?.toString() || ''));

    // State badge
    const stateTd = createEl('td', { className: 'col-state' });
    const stateClass = getNetworkStateClass(conn.state);
    const badge = createEl('span', { className: `state-badge ${stateClass}` }, conn.state || '');
    stateTd.appendChild(badge);
    tr.appendChild(stateTd);

    // Protocol
    tr.appendChild(createEl('td', { className: 'col-protocol' }, conn.protocol || 'TCP'));

    // Process name - clickable to navigate
    const processTd = createEl('td', { className: 'col-process' });
    if (conn.process_name && conn.pid) {
      const processLink = createEl('a', { className: 'nav-link', href: '#', title: 'View process details' }, conn.process_name);
      processLink.addEventListener('click', (e) => {
        e.preventDefault();
        e.stopPropagation();
        navigateToProcess(conn.pid, 'details');
      });
      processTd.appendChild(processLink);
    } else {
      processTd.textContent = conn.process_name || '';
    }
    tr.appendChild(processTd);

    // PID - clickable to navigate
    const pidTd = createEl('td', { className: 'col-pid' });
    if (conn.pid) {
      const pidLink = createEl('a', { className: 'nav-link', href: '#', title: 'View process details' }, conn.pid.toString());
      pidLink.addEventListener('click', (e) => {
        e.preventDefault();
        e.stopPropagation();
        navigateToProcess(conn.pid, 'details');
      });
      pidTd.appendChild(pidLink);
    }
    tr.appendChild(pidTd);

    // Context menu for network tools
    tr.addEventListener('contextmenu', (e) => {
      e.preventDefault();
      selectedConnection = conn;
      showNetworkContextMenu(e.clientX, e.clientY);
    });

    networkTable.appendChild(tr);
  });
}

function getNetworkStateClass(state) {
  if (!state) return '';
  const s = state.toLowerCase();
  if (s === 'established') return 'established';
  if (s === 'listen' || s === 'listening') return 'listening';
  if (s.includes('time_wait') || s.includes('time-wait')) return 'time-wait';
  if (s.includes('close_wait') || s.includes('close-wait')) return 'close-wait';
  return '';
}

// ============================================
// Startup Tab
// ============================================
async function loadStartupApps() {
  try {
    cachedStartupApps = await invoke('get_startup_apps');
    renderStartupTab();
  } catch (e) {
    console.error('Failed to load startup apps:', e);
    cachedStartupApps = [];
    renderStartupTab();
  }
}

function renderStartupTab() {
  if (!startupTable) return;

  const countEl = document.getElementById('startup-count');
  if (countEl) countEl.textContent = `${cachedStartupApps.length} items`;

  // Filter by search
  const query = searchQuery.toLowerCase();
  const filtered = query ? cachedStartupApps.filter(app =>
    app.name?.toLowerCase().includes(query) ||
    app.publisher?.toLowerCase().includes(query) ||
    app.location?.toLowerCase().includes(query)
  ) : cachedStartupApps;

  startupTable.replaceChildren();

  filtered.forEach(app => {
    const tr = createEl('tr');

    // Name
    tr.appendChild(createEl('td', { className: 'col-startup-name' }, app.name || ''));

    // Publisher
    tr.appendChild(createEl('td', { className: 'col-publisher' }, app.publisher || ''));

    // Status with indicator
    const statusTd = createEl('td', { className: 'col-status' });
    const statusDiv = createEl('div', { className: 'startup-status' });
    const indicator = createEl('span', {
      className: `startup-status-indicator ${app.enabled ? 'enabled' : 'disabled'}`
    });
    statusDiv.appendChild(indicator);
    statusDiv.appendChild(document.createTextNode(app.enabled ? 'Enabled' : 'Disabled'));
    statusTd.appendChild(statusDiv);
    tr.appendChild(statusTd);

    // Type
    tr.appendChild(createEl('td', { className: 'col-type' }, app.startup_type || ''));

    // Location (truncated)
    const locTd = createEl('td', { className: 'col-location', title: app.location || '' });
    locTd.textContent = truncateCommand(app.location || '', 40);
    tr.appendChild(locTd);

    startupTable.appendChild(tr);
  });
}

// ============================================
// Per-Core CPU Bars
// ============================================
async function loadCpuCores() {
  try {
    cachedCpuCores = await invoke('get_cpu_cores');
    renderCoreBars();
  } catch (e) {
    console.error('Failed to load CPU cores:', e);
  }
}

function renderCoreBars() {
  if (!coreBars || cachedCpuCores.length === 0) return;

  coreBars.replaceChildren();

  cachedCpuCores.forEach(core => {
    const coreEl = createEl('div', { className: 'core-bar' });

    // Label
    coreEl.appendChild(createEl('div', { className: 'core-bar-label' }, `C${core.core_id}`));

    // Bar container
    const container = createEl('div', { className: 'core-bar-container' });

    // Fill
    const fill = createEl('div', { className: 'core-bar-fill' });
    fill.style.height = `${core.usage_percent}%`;
    container.appendChild(fill);

    // Value overlay
    const value = createEl('div', { className: 'core-bar-value' }, `${core.usage_percent.toFixed(0)}%`);
    container.appendChild(value);

    coreEl.appendChild(container);
    coreBars.appendChild(coreEl);
  });
}

// ============================================
// Process Priority Control
// ============================================
async function setProcessPriority(pid, priority) {
  try {
    await invoke('set_process_priority', { pid, priority });
    console.log(`Set priority of PID ${pid} to ${priority}`);
  } catch (e) {
    console.error('Failed to set process priority:', e);
  }
}

// ============================================
// Debugger Attachment
// ============================================
async function loadDebuggers() {
  try {
    availableDebuggers = await invoke('get_available_debuggers');
    populateDebuggerSubmenu();
  } catch (e) {
    console.error('Failed to load debuggers:', e);
    availableDebuggers = [];
  }
}

function populateDebuggerSubmenu() {
  const submenu = document.getElementById('debugger-submenu');
  if (!submenu) return;

  submenu.replaceChildren();

  if (availableDebuggers.length === 0) {
    const item = createEl('button', { className: 'context-item', disabled: 'true' }, 'No debuggers found');
    submenu.appendChild(item);
    return;
  }

  availableDebuggers.forEach(dbg => {
    const item = createEl('button', { className: 'context-item' }, dbg.name);
    item.addEventListener('click', () => attachDebugger(dbg.id));
    submenu.appendChild(item);
  });
}

async function attachDebugger(debuggerId) {
  if (!selectedPid) return;

  try {
    await invoke('attach_debugger', { pid: selectedPid, debuggerId });
    console.log(`Attached ${debuggerId} to PID ${selectedPid}`);
    hideContextMenu();
  } catch (e) {
    console.error('Failed to attach debugger:', e);
  }
}

// ============================================
// Process Details Panel
// ============================================
async function showProcessDetails(pid) {
  try {
    const details = await invoke('get_process_details', { pid });
    renderProcessDetailsPanel(details);
  } catch (e) {
    console.error('Failed to get process details:', e);
  }
}

function renderProcessDetailsPanel(details) {
  // Create or get the details panel
  let overlay = document.querySelector('.process-details-overlay');
  let panel = document.querySelector('.process-details-panel');

  if (!overlay) {
    overlay = createEl('div', { className: 'process-details-overlay' });
    document.body.appendChild(overlay);

    overlay.addEventListener('click', () => {
      overlay.classList.remove('visible');
      panel.classList.remove('visible');
    });
  }

  if (!panel) {
    panel = createEl('div', { className: 'process-details-panel' });

    const header = createEl('div', { className: 'process-details-header' });
    const closeBtn = createEl('button', { className: 'process-details-close' }, '✕');
    closeBtn.addEventListener('click', () => {
      overlay.classList.remove('visible');
      panel.classList.remove('visible');
    });
    header.appendChild(closeBtn);
    header.appendChild(createEl('span', { className: 'process-details-title', id: 'details-panel-title' }));
    panel.appendChild(header);

    panel.appendChild(createEl('div', { className: 'process-details-content', id: 'details-panel-content' }));
    document.body.appendChild(panel);
  }

  // Update title
  const title = document.getElementById('details-panel-title');
  if (title) title.textContent = `${details.name} (PID: ${details.pid})`;

  // Update content
  const content = document.getElementById('details-panel-content');
  if (content) {
    content.replaceChildren();

    // General section
    const generalSection = createEl('div', { className: 'details-section' });
    generalSection.appendChild(createEl('h4', {}, 'General'));
    addDetailsRow(generalSection, 'Process ID', details.pid);
    addDetailsRow(generalSection, 'Status', details.state || 'Running');
    addDetailsRow(generalSection, 'User', details.user || 'SYSTEM');
    addDetailsRow(generalSection, 'Priority', getPriorityLabel(details.priority));
    content.appendChild(generalSection);

    // Performance section
    const perfSection = createEl('div', { className: 'details-section' });
    perfSection.appendChild(createEl('h4', {}, 'Performance'));
    addDetailsRow(perfSection, 'CPU', `${(details.cpu_percent || 0).toFixed(1)}%`);
    addDetailsRow(perfSection, 'Memory', `${(details.memory_mb || 0).toFixed(1)} MiB`);
    addDetailsRow(perfSection, 'Threads', details.thread_count || 0);
    addDetailsRow(perfSection, 'Handles', details.handle_count || 0);
    content.appendChild(perfSection);

    // I/O section
    const ioSection = createEl('div', { className: 'details-section' });
    ioSection.appendChild(createEl('h4', {}, 'I/O'));
    addDetailsRow(ioSection, 'Disk Read', formatBytes(details.disk_read_bytes || 0));
    addDetailsRow(ioSection, 'Disk Write', formatBytes(details.disk_write_bytes || 0));
    content.appendChild(ioSection);

    // Command section
    const cmdSection = createEl('div', { className: 'details-section' });
    cmdSection.appendChild(createEl('h4', {}, 'Command'));
    const cmdEl = createEl('div', { className: 'details-row' });
    cmdEl.style.wordBreak = 'break-all';
    cmdEl.style.fontSize = '11px';
    cmdEl.textContent = details.command || '';
    cmdSection.appendChild(cmdEl);
    content.appendChild(cmdSection);

    // Threads section (if available)
    if (details.threads && details.threads.length > 0) {
      const threadsSection = createEl('div', { className: 'details-section' });
      threadsSection.appendChild(createEl('h4', {}, `Threads (${details.threads.length})`));
      details.threads.slice(0, 10).forEach(t => {
        addDetailsRow(threadsSection, `Thread ${t.tid}`, t.state || 'Running');
      });
      if (details.threads.length > 10) {
        addDetailsRow(threadsSection, '...', `+${details.threads.length - 10} more`);
      }
      content.appendChild(threadsSection);
    }
  }

  // Show panel
  overlay.classList.add('visible');
  panel.classList.add('visible');
}

function addDetailsRow(container, label, value) {
  const row = createEl('div', { className: 'details-row' });
  row.appendChild(createEl('span', { className: 'details-label' }, label));
  row.appendChild(createEl('span', { className: 'details-value' }, String(value)));
  container.appendChild(row);
}

function getPriorityLabel(priority) {
  const labels = {
    0x00000040: 'Idle',
    0x00004000: 'Below Normal',
    0x00000020: 'Normal',
    0x00008000: 'Above Normal',
    0x00000080: 'High',
    0x00000100: 'Realtime'
  };
  return labels[priority] || 'Normal';
}

function formatBytes(bytes) {
  if (bytes === 0) return '0 B';
  const k = 1024;
  const sizes = ['B', 'KiB', 'MiB', 'GiB', 'TiB'];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${(bytes / Math.pow(k, i)).toFixed(1)} ${sizes[i]}`;
}

// ============================================
// GPU Monitoring
// ============================================
async function loadGpuInfo() {
  try {
    // Parallel fetch - saves ~300ms
    const [gpuInfo, gpuProcs] = await Promise.all([
      invoke('get_gpu_info'),
      invoke('get_gpu_processes')
    ]);
    cachedGpuInfo = gpuInfo;
    cachedGpuProcesses = gpuProcs;
    renderGpuInfo();
  } catch (e) {
    console.error('Failed to load GPU info:', e);
  }
}

function renderGpuInfo() {
  const gpuLarge = document.getElementById('gpu-large');
  const gpuName = document.getElementById('gpu-name');
  const gpuMem = document.getElementById('gpu-mem');
  const gpuTemp = document.getElementById('gpu-temp');
  const gpuProcesses = document.getElementById('gpu-processes');

  if (cachedGpuInfo.length > 0) {
    const gpu = cachedGpuInfo[0];
    if (gpuLarge) gpuLarge.textContent = gpu.utilization.toFixed(0);
    if (gpuName) gpuName.textContent = gpu.name || '--';
    if (gpuMem) gpuMem.textContent = `${gpu.memory_used_mb} / ${gpu.memory_total_mb} MiB`;
    if (gpuTemp) gpuTemp.textContent = gpu.temperature ? `${gpu.temperature}°C` : '--';
  }

  if (gpuProcesses && cachedGpuProcesses.length > 0) {
    gpuProcesses.replaceChildren();
    cachedGpuProcesses.slice(0, 5).forEach(proc => {
      const row = createEl('div', { className: 'gpu-process-row' });

      // Make process name clickable to navigate to Details
      const nameEl = createEl('span', { className: 'gpu-process-name' });
      if (proc.pid) {
        const nameLink = createEl('a', { className: 'nav-link', href: '#', title: 'View process details' }, proc.name);
        nameLink.addEventListener('click', (e) => {
          e.preventDefault();
          navigateToProcess(proc.pid, 'details');
        });
        nameEl.appendChild(nameLink);
      } else {
        nameEl.textContent = proc.name;
      }
      row.appendChild(nameEl);

      if (proc.gpu_percent > 0) {
        row.appendChild(createEl('span', { className: 'gpu-process-usage' }, `${proc.gpu_percent.toFixed(1)}%`));
      }
      if (proc.gpu_memory_mb > 0) {
        row.appendChild(createEl('span', { className: 'gpu-process-mem' }, `${proc.gpu_memory_mb} MiB`));
      }
      gpuProcesses.appendChild(row);
    });
  }
}

// ============================================
// CPU Affinity Control
// ============================================
async function showAffinityDialog(pid) {
  currentAffinityPid = pid;

  try {
    const affinity = await invoke('get_process_affinity', { pid });

    // Find process name
    const proc = cachedProcesses.find(p => p.pid === pid);
    const procName = proc ? proc.short_name : `PID ${pid}`;

    // Update dialog
    const overlay = document.getElementById('affinity-overlay');
    const processInfo = document.getElementById('affinity-process-info');
    const coresContainer = document.getElementById('affinity-cores');

    if (processInfo) processInfo.textContent = `Process: ${procName} (PID: ${pid})`;

    if (coresContainer) {
      coresContainer.replaceChildren();

      for (let i = 0; i < affinity.core_count; i++) {
        const coreDiv = createEl('div', { className: 'affinity-core' });

        const checkbox = createEl('input', {
          type: 'checkbox',
          id: `core-${i}`,
          'data-core': i
        });
        checkbox.checked = affinity.cores[i];

        const label = createEl('label', { for: `core-${i}` }, `CPU ${i}`);

        coreDiv.appendChild(checkbox);
        coreDiv.appendChild(label);
        coresContainer.appendChild(coreDiv);
      }
    }

    if (overlay) overlay.classList.add('visible');
  } catch (e) {
    console.error('Failed to get process affinity:', e);
  }
}

function hideAffinityDialog() {
  const overlay = document.getElementById('affinity-overlay');
  if (overlay) overlay.classList.remove('visible');
  currentAffinityPid = null;
}

async function applyAffinity() {
  if (!currentAffinityPid) return;

  const coresContainer = document.getElementById('affinity-cores');
  if (!coresContainer) return;

  // Calculate mask from checkboxes
  let mask = 0n;
  coresContainer.querySelectorAll('input[type="checkbox"]').forEach((checkbox, i) => {
    if (checkbox.checked) {
      mask |= 1n << BigInt(i);
    }
  });

  if (mask === 0n) {
    console.error('At least one core must be selected');
    return;
  }

  try {
    await invoke('set_process_affinity', { pid: currentAffinityPid, mask: Number(mask) });
    hideAffinityDialog();
  } catch (e) {
    console.error('Failed to set affinity:', e);
  }
}

function selectAllCores(selectAll) {
  const coresContainer = document.getElementById('affinity-cores');
  if (!coresContainer) return;

  coresContainer.querySelectorAll('input[type="checkbox"]').forEach(checkbox => {
    checkbox.checked = selectAll;
  });
}

// Update charts
function updateCharts() {
  const toPoints = (data) => {
    if (data.length === 0) return '';
    const step = 100 / Math.max(data.length - 1, 1);
    return data.map((v, i) => `${i * step},${100 - v}`).join(' ');
  };

  if (cpuLine) cpuLine.setAttribute('points', toPoints(cpuHistory));
  if (memLine) memLine.setAttribute('points', toPoints(memHistory));
}

// Update performance panel
function updatePerformancePanel(stats) {
  const { memory, cpu_percent, load_avg, uptime_secs, kernel, hostname, cpu_count, swap_total_mb, swap_used_mb } = stats;

  const cpuLarge = document.getElementById('cpu-large');
  const memLarge = document.getElementById('mem-large');
  if (cpuLarge) cpuLarge.textContent = cpu_percent.toFixed(1);
  if (memLarge) memLarge.textContent = memory.percent.toFixed(0);

  const loadDisplay = document.getElementById('load-display');
  const memUsed = document.getElementById('mem-used');
  const memAvail = document.getElementById('mem-avail');

  if (loadDisplay) loadDisplay.textContent = load_avg.map(l => l.toFixed(2)).join('  ');
  if (memUsed) memUsed.textContent = `${memory.used_mb.toLocaleString()} / ${memory.total_mb.toLocaleString()} MiB`;
  if (memAvail) memAvail.textContent = `${memory.available_mb.toLocaleString()} MiB`;

  const sysKernel = document.getElementById('sys-kernel');
  const sysHostname = document.getElementById('sys-hostname');
  const sysUptime = document.getElementById('sys-uptime');
  const sysSwap = document.getElementById('sys-swap');
  const cpuCores = document.getElementById('cpu-cores');

  if (sysKernel) sysKernel.textContent = kernel || '--';
  if (sysHostname) sysHostname.textContent = hostname || '--';
  if (sysUptime) sysUptime.textContent = formatUptime(uptime_secs || 0);
  if (sysSwap) sysSwap.textContent = `${swap_used_mb || 0} / ${swap_total_mb || 0} MiB`;
  if (cpuCores) cpuCores.textContent = cpu_count || '--';

  // Top consumers
  const topProcs = [...processes].sort((a, b) => b.memory_mb - a.memory_mb).slice(0, 5);
  const maxMem = Math.max(...topProcs.map(p => p.memory_mb), 1);

  const container = document.getElementById('top-consumers');
  if (container) {
    container.replaceChildren();

    topProcs.forEach(p => {
      const row = createEl('div', { className: 'consumer-row' });
      row.appendChild(createEl('span', { className: 'consumer-name' }, p.short_name));

      const barContainer = createEl('div', { className: 'consumer-bar' });
      const barFill = createEl('div', { className: 'consumer-bar-fill' });
      barFill.style.width = `${(p.memory_mb / maxMem) * 100}%`;
      barContainer.appendChild(barFill);
      row.appendChild(barContainer);

      row.appendChild(createEl('span', { className: 'consumer-value' }, `${p.memory_mb.toFixed(0)} MiB`));
      container.appendChild(row);
    });
  }
}

// Load available system sources
async function loadSources() {
  try {
    availableSources = await invoke('list_sources');
    updateSourceDropdown();
  } catch (e) {
    console.error('Failed to load sources:', e);
  }
}

// Update source dropdown with available sources
function updateSourceDropdown() {
  if (!sourceSelect) return;

  sourceSelect.innerHTML = '';

  availableSources.forEach(source => {
    const option = document.createElement('option');
    option.value = source.id;
    option.textContent = `${source.icon} ${source.name}`;
    option.disabled = !source.is_available && source.source_type === 'wsl';
    if (source.id === currentSource) option.selected = true;
    sourceSelect.appendChild(option);
  });
}

// Fetch stats for current source - non-blocking fire-and-forget pattern
function refresh() {
  // Prevent concurrent refreshes
  if (isRefreshing) return;
  isRefreshing = true;

  // Trigger background refresh of system tree (non-blocking)
  triggerBackgroundRefresh();

  // Non-blocking stats fetch - fire and forget, update when ready
  invoke('get_stats_for_source', { sourceId: currentSource })
    .then(stats => {
      updateStats(stats, currentSource);
      isRefreshing = false;
    })
    .catch(e => {
      console.error('Failed to get stats:', e);
      // Fallback to default WSL stats
      invoke('get_stats')
        .then(stats => {
          updateStats(stats, currentSource);
        })
        .catch(e2 => {
          console.error('Fallback also failed:', e2);
        })
        .finally(() => {
          isRefreshing = false;
        });
    });
}

// Kill handlers
async function killProcess(pid) {
  try {
    await invoke('kill_process', { pid });
    selectedPid = null;
    refresh();
  } catch (e) {
    console.error('Kill failed:', e);
  }
}

async function killTree(pid) {
  try {
    await invoke('kill_tree', { pid });
    selectedPid = null;
    refresh();
  } catch (e) {
    console.error('Kill tree failed:', e);
  }
}

async function killPattern(pattern) {
  try {
    await invoke('kill_pattern', { pattern });
    refresh();
  } catch (e) {
    console.error('Kill pattern failed:', e);
  }
}

// Context menu
function showContextMenu(x, y) {
  if (!contextMenu) return;
  contextMenu.style.left = `${x}px`;
  contextMenu.style.top = `${y}px`;
  contextMenu.classList.add('visible');
}

function hideContextMenu() {
  if (contextMenu) contextMenu.classList.remove('visible');
}

// Network-specific context menu
function showNetworkContextMenu(x, y) {
  const menu = document.getElementById('network-context-menu');
  if (!menu) return;
  menu.style.left = `${x}px`;
  menu.style.top = `${y}px`;
  menu.classList.add('visible');
}

function hideNetworkContextMenu() {
  const menu = document.getElementById('network-context-menu');
  if (menu) menu.classList.remove('visible');
}

// ============================================
// Cross-Tab Navigation
// ============================================

// Navigate to a process in a target tab
function navigateToProcess(pid, targetTab = 'details') {
  if (!pid) return;

  // Set global selection
  selectedPid = pid;

  // Switch tab
  switchToTab(targetTab);

  // Scroll to and highlight the row after render
  setTimeout(() => {
    const selectedRow = document.querySelector(`tr[data-pid="${pid}"], .tree-row[data-pid="${pid}"]`);
    if (selectedRow) {
      selectedRow.scrollIntoView({ behavior: 'smooth', block: 'center' });
      selectedRow.classList.add('highlight-flash');
      setTimeout(() => selectedRow.classList.remove('highlight-flash'), 1000);
    }
  }, 100);
}

// Navigate to network tab filtered by process
function navigateToNetworkForProcess(pid) {
  if (!pid) return;

  // Set search to filter by PID
  const proc = cachedProcesses.find(p => p.pid === pid);
  const filterTerm = proc ? proc.short_name : pid.toString();

  if (searchInput) {
    searchInput.value = filterTerm;
    searchQuery = filterTerm;
  }

  switchToTab('network');
  loadNetworkConnections(); // Refresh to show filtered
}

// Utility to switch tabs programmatically
function switchToTab(tabName) {
  currentTab = tabName;

  // Update sidebar active state
  document.querySelectorAll('.nav-item').forEach(item => {
    item.classList.toggle('active', item.dataset.tab === tabName);
  });

  // Update panel visibility
  document.querySelectorAll('.panel').forEach(panel => {
    panel.classList.toggle('active', panel.id === `${tabName}-panel`);
  });

  // Render the tab content
  if (tabName === 'processes') {
    renderSystemTree();
  } else if (tabName === 'details') {
    renderDetailsTab();
  } else if (tabName === 'performance') {
    if (cachedStats) updatePerformancePanel(cachedStats);
    renderCoreBars();
    renderGpuInfo();
    setTimeout(() => { loadCpuCores(); loadGpuInfo(); }, 100);
  } else if (tabName === 'users') {
    renderUsersTab();
  } else if (tabName === 'services') {
    if ((servicesSource === 'linux' ? cachedLinuxServices : cachedWindowsServices).length === 0) {
      loadServices();
    } else {
      renderServicesTab();
    }
  } else if (tabName === 'network') {
    if (cachedNetworkConnections.length === 0) {
      loadNetworkConnections();
    } else {
      renderNetworkTab();
    }
  } else if (tabName === 'startup') {
    if (cachedStartupApps.length === 0) {
      loadStartupApps();
    } else {
      renderStartupTab();
    }
  } else if (tabName === 'explore') {
    renderExploreTab();
    loadExploreData();
  }
}

// ============================================
// Explore Tab - System DevTools
// ============================================

// Explore tab state
let exploreHeatmapData = null;
let profiledProcess = null;
let profilerInterval = null;
const profilerCpuHistory = [];
const profilerMemHistory = [];
const MAX_PROFILER_HISTORY = 60;

// Colors for memory blocks (consistent hashing)
const MEMORY_COLORS = [
  'oklch(0.55 0.18 145)', // green
  'oklch(0.55 0.18 220)', // blue
  'oklch(0.55 0.18 280)', // purple
  'oklch(0.55 0.18 340)', // magenta
  'oklch(0.55 0.18 30)',  // orange
  'oklch(0.55 0.18 60)',  // yellow
  'oklch(0.55 0.18 180)', // cyan
];

function getProcessColor(name) {
  let hash = 0;
  for (let i = 0; i < name.length; i++) {
    hash = ((hash << 5) - hash) + name.charCodeAt(i);
    hash |= 0;
  }
  return MEMORY_COLORS[Math.abs(hash) % MEMORY_COLORS.length];
}

// Render the Explore tab with cached data
function renderExploreTab() {
  // Populate process dropdown for profiler
  const profilerSelect = document.getElementById('profiler-process');
  if (profilerSelect && cachedProcesses.length > 0) {
    const currentVal = profilerSelect.value;
    profilerSelect.innerHTML = '<option value="">Select a process...</option>';

    // Sort by memory (top consumers first)
    const sorted = [...cachedProcesses]
      .filter(p => p.memory_mb > 10)
      .sort((a, b) => b.memory_mb - a.memory_mb)
      .slice(0, 50);

    sorted.forEach(p => {
      const opt = document.createElement('option');
      opt.value = `${p.pid}:${p.source}`;
      opt.textContent = `${p.short_name} (PID: ${p.pid}) - ${p.memory_mb.toFixed(0)} MB`;
      profilerSelect.appendChild(opt);
    });

    // Restore selection if still valid
    if (currentVal) {
      profilerSelect.value = currentVal;
    }
  }
}

// Load explore data from backend
async function loadExploreData() {
  try {
    const heatmap = await invoke('get_system_heatmap');
    exploreHeatmapData = heatmap;
    renderCpuHeatmap(heatmap);
    renderMemoryVisualization(heatmap);
  } catch (e) {
    console.error('Failed to load explore data:', e);
  }
}

// Render CPU core heatmap
function renderCpuHeatmap(heatmap) {
  const container = document.getElementById('cpu-heatmap');
  const infoEl = document.getElementById('cpu-arch-info');
  if (!container || !heatmap) return;

  infoEl.textContent = `${heatmap.cores.length} cores`;
  container.innerHTML = '';

  heatmap.cores.forEach((core, idx) => {
    const cell = createEl('div', { className: 'cpu-core-cell' });

    // Determine heat class
    const usage = core.usage;
    let heatClass = 'idle';
    if (usage > 75) heatClass = 'crit';
    else if (usage > 50) heatClass = 'high';
    else if (usage > 25) heatClass = 'med';
    else if (usage > 1) heatClass = 'low';
    cell.classList.add(heatClass);

    cell.appendChild(createEl('div', { className: 'cpu-core-label' }, `Core ${idx}`));
    cell.appendChild(createEl('div', { className: 'cpu-core-usage' }, `${usage.toFixed(0)}%`));

    // Show top process on this core if available
    if (core.top_process) {
      cell.appendChild(createEl('div', { className: 'cpu-core-process' }, core.top_process));
    }

    container.appendChild(cell);
  });
}

// Render memory visualization
function renderMemoryVisualization(heatmap) {
  const barContainer = document.getElementById('memory-bar');
  const breakdownContainer = document.getElementById('memory-breakdown');
  const infoEl = document.getElementById('mem-arch-info');
  if (!barContainer || !heatmap) return;

  const totalGb = (heatmap.memory.total_mb / 1024).toFixed(1);
  const usedGb = (heatmap.memory.used_mb / 1024).toFixed(1);
  infoEl.textContent = `${usedGb} / ${totalGb} GB`;

  barContainer.innerHTML = '';
  breakdownContainer.innerHTML = '';

  // Sort processes by memory
  const topProcesses = [...heatmap.memory.top_processes]
    .sort((a, b) => b.memory_mb - a.memory_mb)
    .slice(0, 10);

  const totalMemory = heatmap.memory.total_mb;
  let accountedMb = 0;

  topProcesses.forEach(proc => {
    const pct = (proc.memory_mb / totalMemory) * 100;
    if (pct < 0.5) return; // Skip tiny processes

    const color = getProcessColor(proc.name);
    accountedMb += proc.memory_mb;

    // Memory bar block
    const block = createEl('div', {
      className: 'memory-block',
      style: `width: ${pct}%; background: ${color};`,
      title: `${proc.name}: ${proc.memory_mb.toFixed(0)} MB`
    });
    if (pct > 5) {
      block.appendChild(createEl('span', { className: 'memory-block-label' }, proc.name));
    }
    block.addEventListener('click', () => {
      if (proc.pid) navigateToProcess(proc.pid, 'details');
    });
    barContainer.appendChild(block);

    // Breakdown list item
    const consumer = createEl('div', { className: 'memory-consumer' });
    consumer.appendChild(createEl('div', {
      className: 'memory-consumer-color',
      style: `background: ${color};`
    }));
    const info = createEl('div', { className: 'memory-consumer-info' });
    info.appendChild(createEl('div', { className: 'memory-consumer-name' }, proc.name));
    info.appendChild(createEl('div', { className: 'memory-consumer-size' },
      `${proc.memory_mb.toFixed(0)} MB (${pct.toFixed(1)}%)`
    ));
    consumer.appendChild(info);
    consumer.addEventListener('click', () => {
      if (proc.pid) navigateToProcess(proc.pid, 'details');
    });
    breakdownContainer.appendChild(consumer);
  });

  // Add "Other" block for remaining memory
  const otherMb = heatmap.memory.used_mb - accountedMb;
  if (otherMb > 0) {
    const otherPct = (otherMb / totalMemory) * 100;
    if (otherPct > 0.5) {
      const otherBlock = createEl('div', {
        className: 'memory-block cached',
        style: `width: ${otherPct}%;`,
        title: `Other: ${otherMb.toFixed(0)} MB`
      });
      barContainer.appendChild(otherBlock);
    }
  }

  // Add free memory block
  const freeMb = heatmap.memory.available_mb;
  const freePct = (freeMb / totalMemory) * 100;
  if (freePct > 0.5) {
    const freeBlock = createEl('div', {
      className: 'memory-block free',
      style: `width: ${freePct}%;`,
      title: `Free: ${freeMb.toFixed(0)} MB`
    });
    barContainer.appendChild(freeBlock);
  }
}

// Start profiling a process
async function startProfiling(pidSource) {
  stopProfiling();

  const [pidStr, source] = pidSource.split(':');
  const pid = parseInt(pidStr);
  if (!pid || isNaN(pid)) return;

  profiledProcess = { pid, source };
  profilerCpuHistory.length = 0;
  profilerMemHistory.length = 0;

  document.getElementById('profiler-content').style.display = 'none';
  document.getElementById('profiler-graphs').style.display = 'grid';

  // Sample every 500ms
  profilerInterval = setInterval(() => sampleProcess(pid, source), 500);
  sampleProcess(pid, source); // Initial sample
}

// Stop profiling
function stopProfiling() {
  if (profilerInterval) {
    clearInterval(profilerInterval);
    profilerInterval = null;
  }
  profiledProcess = null;
}

// Sample process data
async function sampleProcess(pid, source) {
  try {
    const sample = await invoke('get_process_timeline_sample', { pid, source });

    // Update history
    profilerCpuHistory.push(sample.cpu_percent);
    profilerMemHistory.push(sample.memory_mb);

    if (profilerCpuHistory.length > MAX_PROFILER_HISTORY) {
      profilerCpuHistory.shift();
      profilerMemHistory.shift();
    }

    // Update display
    document.getElementById('profiler-cpu-value').textContent = `${sample.cpu_percent.toFixed(1)}%`;
    document.getElementById('profiler-mem-value').textContent = `${sample.memory_mb.toFixed(0)} MB`;
    document.getElementById('profiler-threads-value').textContent = sample.threads || '--';

    // Update sparklines
    updateProfilerSparkline('profiler-cpu-line', profilerCpuHistory, 100);
    updateProfilerSparkline('profiler-mem-line', profilerMemHistory, Math.max(...profilerMemHistory) * 1.1);
  } catch (e) {
    console.error('Failed to sample process:', e);
    stopProfiling();
  }
}

// Update profiler sparkline
function updateProfilerSparkline(lineId, history, maxVal) {
  const line = document.getElementById(lineId);
  if (!line || history.length === 0) return;

  const points = history.map((val, i) => {
    const x = (i / (MAX_PROFILER_HISTORY - 1)) * 200;
    const y = 40 - (val / maxVal) * 38;
    return `${x},${y}`;
  }).join(' ');

  line.setAttribute('points', points);
}

// Launch profiling tool
async function launchProfilerTool(tool, pid, source) {
  const toolCommands = {
    perf: ['wsl', '-e', 'perf', 'top', '-p', pid.toString()],
    strace: ['wsl', '-e', 'strace', '-p', pid.toString()],
    ltrace: ['wsl', '-e', 'ltrace', '-p', pid.toString()],
    valgrind: ['wsl', '-e', 'valgrind', '--tool=memcheck', '--attach=yes', '-p', pid.toString()],
    procmon: ['powershell.exe', '-Command', 'Start-Process', 'procmon', '-Verb', 'RunAs'],
    xperf: ['powershell.exe', '-Command', 'Start-Process', 'wpa', '-Verb', 'RunAs']
  };

  const cmd = toolCommands[tool];
  if (cmd) {
    try {
      await invoke('launch_network_tool', { tool: cmd[0], args: cmd.slice(1) });
    } catch (e) {
      console.error(`Failed to launch ${tool}:`, e);
    }
  }
}

// Initialize explore tab event listeners
function initExploreTab() {
  const profilerSelect = document.getElementById('profiler-process');
  const attachBtn = document.getElementById('profiler-attach');

  if (profilerSelect && attachBtn) {
    attachBtn.addEventListener('click', () => {
      const val = profilerSelect.value;
      if (val) {
        startProfiling(val);
      }
    });
  }

  // Tool buttons
  const toolBtns = [
    ['tool-perf', 'perf'],
    ['tool-strace', 'strace'],
    ['tool-ltrace', 'ltrace'],
    ['tool-valgrind', 'valgrind'],
    ['tool-procmon', 'procmon'],
    ['tool-xperf', 'xperf']
  ];

  toolBtns.forEach(([id, tool]) => {
    const btn = document.getElementById(id);
    if (btn) {
      btn.addEventListener('click', () => {
        if (profiledProcess) {
          launchProfilerTool(tool, profiledProcess.pid, profiledProcess.source);
        } else {
          // Just launch the tool without a PID
          launchProfilerTool(tool, 0, 'windows');
        }
      });
    }
  });
}

// ============================================
// Network Tool Integration
// ============================================

// Selected network connection for context menu
let selectedConnection = null;

// Launch a network tool in Windows Terminal
async function launchNetworkTool(tool, args) {
  try {
    await invoke('launch_network_tool', { tool, args });
  } catch (e) {
    console.error(`Failed to launch ${tool}:`, e);
  }
}

// Quick whois lookup
async function whoisLookup(ip) {
  try {
    const result = await invoke('whois_lookup', { ip });
    showWhoisResult(ip, result);
  } catch (e) {
    console.error('Whois lookup failed:', e);
  }
}

// Show whois result in modal
function showWhoisResult(ip, result) {
  let modal = document.getElementById('whois-modal');
  if (!modal) {
    // Create modal if it doesn't exist
    const overlay = createEl('div', { className: 'modal-overlay', id: 'whois-overlay' });
    modal = createEl('div', { className: 'modal-dialog whois-modal', id: 'whois-modal' });

    const header = createEl('div', { className: 'modal-header' });
    header.appendChild(createEl('span', { className: 'modal-title', id: 'whois-title' }, 'Whois Lookup'));
    const closeBtn = createEl('button', { className: 'modal-close' }, '✕');
    closeBtn.addEventListener('click', () => overlay.classList.remove('visible'));
    header.appendChild(closeBtn);
    modal.appendChild(header);

    modal.appendChild(createEl('pre', { className: 'whois-content', id: 'whois-content' }));
    overlay.appendChild(modal);
    document.body.appendChild(overlay);

    overlay.addEventListener('click', (e) => {
      if (e.target === overlay) overlay.classList.remove('visible');
    });
  }

  document.getElementById('whois-title').textContent = `Whois: ${ip}`;
  document.getElementById('whois-content').textContent = result;
  document.getElementById('whois-overlay').classList.add('visible');
}

// Copy connection info to clipboard
function copyConnectionInfo(conn) {
  const info = `${conn.local_addr}:${conn.local_port} → ${conn.remote_addr}:${conn.remote_port} (${conn.state})`;
  navigator.clipboard.writeText(info).then(() => {
    console.log('Connection info copied');
  });
}

// Event listeners
document.addEventListener('DOMContentLoaded', async () => {
  // Initialize window handle
  appWindow = getCurrentWindow();

  // FAST STARTUP: Load settings and cached data first (sync, <5ms)
  loadSettings();
  const hadCache = restoreCachedSession();

  // Render immediately from cache (before any async work)
  if (hadCache) {
    if (cachedStats) {
      // Update header stats from cache
      const { memory, cpu_percent, load_avg } = cachedStats;
      if (memPct) memPct.textContent = `${memory?.percent?.toFixed(0) || '--'}%`;
      if (cpuPct) cpuPct.textContent = `${cpu_percent?.toFixed(1) || '--'}%`;
      if (sidebarCpu) sidebarCpu.textContent = `${cpu_percent?.toFixed(1) || '--'}%`;
      if (sidebarMem) sidebarMem.textContent = `${memory?.percent?.toFixed(0) || '--'}%`;
    }
    // Render tree/table from cache
    renderSystemTree();
    renderDetailsTab();
  }

  // Show window immediately after initial render (<20ms from start)
  requestAnimationFrame(() => {
    appWindow.show();
  });

  // Disable default browser context menu everywhere - use custom menus only
  document.addEventListener('contextmenu', (e) => {
    e.preventDefault();
  });

  // Disable autofill/autocomplete on all inputs
  document.querySelectorAll('input').forEach(input => {
    input.setAttribute('autocomplete', 'off');
    input.setAttribute('autocorrect', 'off');
    input.setAttribute('autocapitalize', 'off');
    input.setAttribute('spellcheck', 'false');
    input.setAttribute('data-form-type', 'other');
    input.setAttribute('data-lpignore', 'true'); // LastPass
    input.setAttribute('data-1p-ignore', 'true'); // 1Password
  });

  // Window controls
  const winMinimize = document.getElementById('win-minimize');
  const winMaximize = document.getElementById('win-maximize');
  const winClose = document.getElementById('win-close');

  if (winMinimize) winMinimize.addEventListener('click', () => appWindow.minimize());
  if (winMaximize) {
    winMaximize.addEventListener('click', async () => {
      const isMax = await appWindow.isMaximized();
      if (isMax) appWindow.unmaximize();
      else appWindow.maximize();
    });
  }
  // INSTANT SHOW: Hide to tray instead of close (next show is <5ms)
  if (winClose) {
    winClose.addEventListener('click', () => {
      // Save session before hiding for instant restore
      saveSessionCache();
      appWindow.hide();
    });
  }

  // Sidebar navigation - instant tab switching with cached data
  document.querySelectorAll('.nav-item').forEach(item => {
    item.addEventListener('click', () => {
      document.querySelectorAll('.nav-item').forEach(i => i.classList.remove('active'));
      document.querySelectorAll('.panel').forEach(p => p.classList.remove('active'));
      item.classList.add('active');
      currentTab = item.dataset.tab;
      const panel = document.getElementById(`${currentTab}-panel`);
      if (panel) panel.classList.add('active');

      // Instantly render tab content from cached data
      if (currentTab === 'processes') {
        renderSystemTree();
      } else if (currentTab === 'details') {
        renderDetailsTab();
      } else if (currentTab === 'performance') {
        if (cachedStats) updatePerformancePanel(cachedStats);
        // Render from cache first (instant), then refresh in background
        renderCoreBars();
        renderGpuInfo();
        // Background refresh (don't block tab switch)
        setTimeout(() => { loadCpuCores(); loadGpuInfo(); }, 100);
      } else if (currentTab === 'users') {
        renderUsersTab();
      } else if (currentTab === 'services') {
        const services = servicesSource === 'linux' ? cachedLinuxServices : cachedWindowsServices;
        if (services.length === 0) {
          loadServices();
        } else {
          renderServicesTab();
        }
      } else if (currentTab === 'network') {
        if (cachedNetworkConnections.length === 0) {
          loadNetworkConnections();
        } else {
          renderNetworkTab();
        }
      } else if (currentTab === 'startup') {
        if (cachedStartupApps.length === 0) {
          loadStartupApps();
        } else {
          renderStartupTab();
        }
      } else if (currentTab === 'explore') {
        renderExploreTab();
        loadExploreData();
      }
    });
  });

  // Column sorting for Details table
  document.querySelectorAll('th[data-sort]').forEach(th => {
    th.addEventListener('click', () => {
      const col = th.dataset.sort;
      if (sortBy === col) {
        sortAsc = !sortAsc;
      } else {
        sortBy = col;
        sortAsc = false;
      }
      document.querySelectorAll('th').forEach(t => t.classList.remove('sorted', 'asc'));
      th.classList.add('sorted');
      if (sortAsc) th.classList.add('asc');
      renderDetailsTab();
    });
  });

  // Column sorting for Tree view
  document.querySelectorAll('.tree-col[data-sort]').forEach(col => {
    col.addEventListener('click', () => {
      const sortKey = col.dataset.sort;
      if (treeSortBy === sortKey) {
        treeSortAsc = !treeSortAsc;
      } else {
        treeSortBy = sortKey;
        treeSortAsc = false;
      }
      document.querySelectorAll('.tree-col').forEach(c => c.classList.remove('sorted', 'asc'));
      col.classList.add('sorted');
      if (treeSortAsc) col.classList.add('asc');
      renderSystemTree();
    });
  });

  // Process selection - Processes tab
  if (processTable) {
    processTable.addEventListener('click', (e) => {
      const row = e.target.closest('tr:not(.group-header)');
      if (row && !e.target.classList.contains('tree-toggle') && !e.target.classList.contains('group-toggle')) {
        const pid = parseInt(row.dataset.pid);
        if (!isNaN(pid)) {
          selectedPid = selectedPid === pid ? null : pid;
          renderProcessesTab();
          renderDetailsTab();
        }
      }
    });

    processTable.addEventListener('contextmenu', (e) => {
      const row = e.target.closest('tr:not(.group-header)');
      if (row) {
        e.preventDefault();
        const pid = parseInt(row.dataset.pid);
        if (!isNaN(pid)) {
          selectedPid = pid;
          renderProcessesTab();
          renderDetailsTab();
          showContextMenu(e.clientX, e.clientY);
        }
      }
    });
  }

  // Process selection - Details tab
  if (detailsTable) {
    detailsTable.addEventListener('click', (e) => {
      const row = e.target.closest('tr');
      if (row) {
        const pid = parseInt(row.dataset.pid);
        if (!isNaN(pid)) {
          selectedPid = selectedPid === pid ? null : pid;
          renderProcessesTab();
          renderDetailsTab();
        }
      }
    });

    detailsTable.addEventListener('contextmenu', (e) => {
      const row = e.target.closest('tr');
      if (row) {
        e.preventDefault();
        const pid = parseInt(row.dataset.pid);
        if (!isNaN(pid)) {
          selectedPid = pid;
          renderProcessesTab();
          renderDetailsTab();
          showContextMenu(e.clientX, e.clientY);
        }
      }
    });
  }

  // Hide context menus on click outside
  document.addEventListener('click', (e) => {
    if (!contextMenu?.contains(e.target)) hideContextMenu();
    const netMenu = document.getElementById('network-context-menu');
    if (netMenu && !netMenu.contains(e.target)) hideNetworkContextMenu();
  });

  // Context menu actions
  document.getElementById('ctx-end-task')?.addEventListener('click', () => { if (selectedPid) killProcess(selectedPid); hideContextMenu(); });
  document.getElementById('ctx-end-tree')?.addEventListener('click', () => { if (selectedPid) killTree(selectedPid); hideContextMenu(); });
  document.getElementById('ctx-kill-node')?.addEventListener('click', () => { killPattern('node'); hideContextMenu(); });
  document.getElementById('ctx-kill-php')?.addEventListener('click', () => { killPattern('php'); hideContextMenu(); });

  // Network context menu actions
  document.getElementById('net-ctx-copy')?.addEventListener('click', () => {
    if (selectedConnection) copyConnectionInfo(selectedConnection);
    hideNetworkContextMenu();
  });

  document.getElementById('net-ctx-view-process')?.addEventListener('click', () => {
    if (selectedConnection?.pid) navigateToProcess(selectedConnection.pid, 'details');
    hideNetworkContextMenu();
  });

  document.getElementById('net-ctx-tshark')?.addEventListener('click', () => {
    if (selectedConnection) {
      const port = selectedConnection.local_port || selectedConnection.remote_port;
      launchNetworkTool('tshark', ['-i', 'any', '-f', `tcp port ${port}`, '-c', '100']);
    }
    hideNetworkContextMenu();
  });

  document.getElementById('net-ctx-lsof')?.addEventListener('click', () => {
    if (selectedConnection?.pid) {
      launchNetworkTool('lsof', ['-p', selectedConnection.pid.toString(), '-i', '-n', '-P']);
    }
    hideNetworkContextMenu();
  });

  document.getElementById('net-ctx-ss')?.addEventListener('click', () => {
    if (selectedConnection?.pid) {
      launchNetworkTool('ss', ['-tlnp', '|', 'grep', selectedConnection.pid.toString()]);
    }
    hideNetworkContextMenu();
  });

  document.getElementById('net-ctx-nmap')?.addEventListener('click', () => {
    if (selectedConnection?.remote_addr && selectedConnection.remote_addr !== '*') {
      const ports = selectedConnection.remote_port ? `-p ${selectedConnection.remote_port}` : '-F';
      launchNetworkTool('nmap', ['-sT', '-Pn', selectedConnection.remote_addr, ports]);
    }
    hideNetworkContextMenu();
  });

  document.getElementById('net-ctx-whois')?.addEventListener('click', () => {
    if (selectedConnection?.remote_addr && selectedConnection.remote_addr !== '*') {
      whoisLookup(selectedConnection.remote_addr);
    }
    hideNetworkContextMenu();
  });

  // Services refresh button
  document.getElementById('services-refresh')?.addEventListener('click', loadServices);

  // Services source switcher
  document.getElementById('services-source')?.addEventListener('change', (e) => {
    servicesSource = e.target.value;
    const services = servicesSource === 'linux' ? cachedLinuxServices : cachedWindowsServices;
    if (services.length === 0) {
      loadServices();
    } else {
      renderServicesTab();
    }
  });

  // Network refresh button
  document.getElementById('network-refresh')?.addEventListener('click', loadNetworkConnections);

  // Startup refresh button
  document.getElementById('startup-refresh')?.addEventListener('click', loadStartupApps);

  // Priority submenu handlers
  document.querySelectorAll('#priority-submenu [data-priority]').forEach(btn => {
    btn.addEventListener('click', () => {
      if (selectedPid) {
        setProcessPriority(selectedPid, btn.dataset.priority);
        hideContextMenu();
      }
    });
  });

  // View details context menu item
  document.getElementById('ctx-details')?.addEventListener('click', () => {
    if (selectedPid) {
      showProcessDetails(selectedPid);
      hideContextMenu();
    }
  });

  // Affinity context menu item
  document.getElementById('ctx-affinity')?.addEventListener('click', () => {
    if (selectedPid) {
      showAffinityDialog(selectedPid);
      hideContextMenu();
    }
  });

  // Affinity dialog handlers
  document.getElementById('affinity-close')?.addEventListener('click', hideAffinityDialog);
  document.getElementById('affinity-cancel')?.addEventListener('click', hideAffinityDialog);
  document.getElementById('affinity-apply')?.addEventListener('click', applyAffinity);
  document.getElementById('affinity-all')?.addEventListener('click', () => selectAllCores(true));
  document.getElementById('affinity-none')?.addEventListener('click', () => selectAllCores(false));
  document.getElementById('affinity-overlay')?.addEventListener('click', (e) => {
    if (e.target.id === 'affinity-overlay') hideAffinityDialog();
  });

  // Settings handlers
  document.getElementById('setting-refresh-rate')?.addEventListener('change', (e) => {
    settings.refreshRate = parseInt(e.target.value);
    saveSettings();
    updateRefreshInterval();
  });

  document.getElementById('setting-default-tab')?.addEventListener('change', (e) => {
    settings.defaultTab = e.target.value;
    saveSettings();
  });

  document.getElementById('setting-show-system')?.addEventListener('change', (e) => {
    settings.showSystem = e.target.checked;
    saveSettings();
    // Re-render to apply filter
    renderSystemTree();
    renderDetailsTab();
  });

  document.getElementById('setting-compact-mode')?.addEventListener('change', (e) => {
    settings.compactMode = e.target.checked;
    saveSettings();
    applySettings();
  });

  document.getElementById('setting-confirm-kill')?.addEventListener('change', (e) => {
    settings.confirmKill = e.target.checked;
    saveSettings();
  });

  document.getElementById('setting-start-minimized')?.addEventListener('change', (e) => {
    settings.startMinimized = e.target.checked;
    saveSettings();
  });

  document.getElementById('setting-autostart')?.addEventListener('change', (e) => {
    settings.autostart = e.target.checked;
    saveSettings();
    // TODO: Register/unregister with Windows startup
  });

  // Search - contextual filtering based on current tab
  if (searchInput) {
    searchInput.addEventListener('input', (e) => {
      searchQuery = e.target.value;
      // Instant render - no backend calls, just filter cached data
      if (currentTab === 'processes') {
        renderSystemTree();
      } else if (currentTab === 'details') {
        renderDetailsTab();
      } else if (currentTab === 'users') {
        renderUsersTab();
      } else if (currentTab === 'services') {
        renderServicesTab();
      } else if (currentTab === 'network') {
        renderNetworkTab();
      } else if (currentTab === 'startup') {
        renderStartupTab();
      }
    });
  }

  // Header buttons
  document.getElementById('refresh-btn')?.addEventListener('click', refresh);
  killBtn?.addEventListener('click', () => { if (selectedPid) killProcess(selectedPid); });

  // Source selector - instant switching using cached data
  if (sourceSelect) {
    sourceSelect.addEventListener('change', (e) => {
      currentSource = e.target.value;
      cpuHistory.length = 0;
      memHistory.length = 0;

      // Instant switch: use cached data if available
      if (cachedStatsBySource.has(currentSource)) {
        const cached = cachedStatsBySource.get(currentSource);
        processes = cached.processes || [];
        cachedProcesses = processes;
        cachedStats = cached;
        // Re-render immediately with cached data
        renderSystemTree();
        renderDetailsTab();
        updatePerformancePanel(cached);
      } else if (currentSource !== 'all' && allProcesses.length > 0) {
        // Filter from allProcesses for instant display
        processes = allProcesses.filter(p => p.source === currentSource);
        cachedProcesses = processes;
        renderSystemTree();
        renderDetailsTab();
      }

      // Then trigger background refresh to get fresh data (non-blocking)
      isRefreshing = false; // Reset to allow refresh
      refresh();
    });
  }

  // Listen for refresh events from tray
  listen('refresh', refresh);

  // Simple debounced resize handler for chart updates
  let resizeTimer = null;
  window.addEventListener('resize', () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(updateCharts, 150);
  });

  // Switch to default tab from settings (instant, no async)
  if (settings.defaultTab !== 'processes') {
    const navItem = document.querySelector(`.nav-item[data-tab="${settings.defaultTab}"]`);
    if (navItem) navItem.click();
  }

  // Initialize Explore tab event handlers
  initExploreTab();

  // DEFERRED STARTUP: Load fresh data after window is visible
  deferWork(async () => {
    // Load sources (fast operation)
    await loadSources();

    // Load debuggers list for context menu
    loadDebuggers();

    // Start refresh cycle
    refresh();
    updateRefreshInterval();

    // Background pre-fetch all tabs (fire and forget - don't block)
    // This ensures instant tab switching after first load
    setTimeout(() => {
      if (cachedWindowsServices.length === 0) loadServices();
    }, 500);
    setTimeout(() => {
      if (cachedNetworkConnections.length === 0) loadNetworkConnections();
    }, 1000);
    setTimeout(() => {
      if (cachedStartupApps.length === 0) loadStartupApps();
    }, 1500);

    // Periodically refresh sources
    setInterval(loadSources, 30000);

    // Periodically refresh CPU cores and GPU when on performance tab
    setInterval(() => {
      if (currentTab === 'performance') {
        loadCpuCores();
        loadGpuInfo();
      }
    }, 2000);

    // Save session cache periodically for next startup
    setInterval(saveSessionCache, 10000);
  });
});
