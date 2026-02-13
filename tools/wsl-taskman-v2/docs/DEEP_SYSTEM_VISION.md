# Deep System Visualization Architecture

## Vision
Transform WSL Task Manager into a real-time system observatory - visualizing data flow from disk through memory hierarchy to CPU execution.

## Data Sources (Windows APIs)

### CPU Cache & Performance Counters
```rust
// Windows Performance Data Helper (PDH)
use windows::Win32::System::Performance::*;

// Counters to query:
// \Processor Information(*)\% Processor Performance
// \Memory\Cache Bytes
// \Memory\Cache Faults/sec
// \Memory\Page Faults/sec
// \PhysicalDisk(*)\Disk Bytes/sec
// \PhysicalDisk(*)\Current Disk Queue Length
```

### Cache Topology (CPUID instruction)
```rust
// Use x86 CPUID to get cache info
// EAX=4: Deterministic Cache Parameters
// Returns: cache level, type, size, line size, associativity

struct CacheInfo {
    level: u8,           // L1, L2, L3
    cache_type: String,  // Data, Instruction, Unified
    size_kb: u32,
    line_size: u32,
    associativity: u32,
    partitions: u32,
}
```

### Memory Flow Tracking
```rust
// Page fault monitoring
// Working set tracking per process
// DMA transfer rates

struct MemoryFlow {
    page_faults_per_sec: f64,
    hard_faults_per_sec: f64,  // Disk reads
    soft_faults_per_sec: f64,  // Already in RAM
    working_set_mb: u64,
    commit_mb: u64,
}
```

### Disk I/O
```rust
// Per-disk and per-process I/O stats
struct DiskFlow {
    read_bytes_per_sec: u64,
    write_bytes_per_sec: u64,
    queue_depth: u32,
    latency_ms: f64,
    iops: u32,
}
```

## Visualization Components

### 1. Memory Hierarchy View
```
┌────────────────────────────────────────────────────────┐
│  CPU Core 0                    CPU Core 1              │
│  ┌─────────┐ ┌─────────┐      ┌─────────┐ ┌─────────┐  │
│  │ L1d 32K │ │ L1i 32K │      │ L1d 32K │ │ L1i 32K │  │
│  │ █████░░ │ │ ████░░░ │      │ ██████░ │ │ █████░░ │  │
│  └────┬────┘ └────┬────┘      └────┬────┘ └────┬────┘  │
│       └─────┬─────┘                └─────┬─────┘       │
│        ┌────┴────┐                  ┌────┴────┐        │
│        │ L2 256K │                  │ L2 256K │        │
│        │ ████░░░ │                  │ █████░░ │        │
│        └────┬────┘                  └────┬────┘        │
│             └──────────┬──────────────┘               │
│                   ┌────┴────┐                          │
│                   │  L3 8MB │                          │
│                   │ ████████████░░░░░░░░ │             │
│                   └────┬────┘                          │
└────────────────────────┼───────────────────────────────┘
                         │
              ┌──────────┴──────────┐
              │        RAM          │
              │ ████████████░░░░░░░ │ 12GB/16GB
              └──────────┬──────────┘
                         │ ↑ 340MB/s  ↓ 120MB/s
              ┌──────────┴──────────┐
              │     NVMe SSD        │
              │ Read: 2.1GB/s       │
              └─────────────────────┘
```

### 2. Data Flow Animation
- Particles flowing from disk → RAM → L3 → L2 → L1 → CPU
- Color-coded by process (highlight selected process)
- Speed proportional to actual transfer rate

### 3. CPU Pipeline View
```
┌─────────────────────────────────────────────────────────┐
│  FETCH → DECODE → EXECUTE → MEMORY → WRITEBACK         │
│  ┌───┐   ┌───┐    ┌───┐     ┌───┐    ┌───┐            │
│  │ I │→→→│ D │→→→→│ E │→→→→→│ M │→→→→│ W │            │
│  └───┘   └───┘    └───┘     └───┘    └───┘            │
│                                                         │
│  Current: ADD RAX, RBX     IPC: 2.4    Stalls: 12%    │
└─────────────────────────────────────────────────────────┘
```

### 4. Assembly/Instruction View
```
┌─────────────────────────────────────────────────────────┐
│  Hot Instructions (chrome.exe:main)                     │
│  ──────────────────────────────────────────────────────│
│  0x7ff6`12345678  mov    rax, [rbx+8]     │ 12.3%      │
│  0x7ff6`1234567c  add    rax, rcx         │  8.1%      │
│  0x7ff6`12345680  cmp    rax, 0x1000      │  5.2%      │
│  0x7ff6`12345687  jne    0x7ff6`12345690  │  4.8%      │
│  ...                                                    │
└─────────────────────────────────────────────────────────┘
```

## Implementation Plan

### Phase 1: Cache Topology (read-only)
- [ ] Query CPUID for cache hierarchy
- [ ] Display L1/L2/L3 sizes per core
- [ ] Show as interactive diagram

### Phase 2: Performance Counters
- [ ] Initialize PDH queries
- [ ] Real-time cache hit/miss rates
- [ ] Memory bandwidth utilization
- [ ] Disk I/O rates

### Phase 3: Flow Visualization
- [ ] WebGL/Canvas animated particles
- [ ] Data flowing through hierarchy
- [ ] Process-colored streams

### Phase 4: Assembly Integration
- [ ] Use Windows DbgHelp for symbol resolution
- [ ] ETW (Event Tracing for Windows) for instruction sampling
- [ ] Hot path identification

## Rust Backend Additions

```rust
// New commands for Explore tab
#[tauri::command]
async fn get_cache_topology() -> Result<Vec<CacheInfo>, String>

#[tauri::command]
async fn get_memory_flow() -> Result<MemoryFlow, String>

#[tauri::command]
async fn get_disk_flow() -> Result<Vec<DiskFlow>, String>

#[tauri::command]
async fn get_hot_instructions(pid: u32) -> Result<Vec<Instruction>, String>
```

## Required Windows Features
```toml
[target.'cfg(windows)'.dependencies]
windows = { version = "0.58", features = [
    # Existing...
    "Win32_System_Performance",      # PDH counters
    "Win32_System_Diagnostics_Etw",  # Event tracing
    "Win32_System_Diagnostics_Debug", # DbgHelp
] }
```

## Frontend Components

```
src/lib/tabs/Explore.svelte
├── CacheHierarchy.svelte      # Interactive cache diagram
├── MemoryFlowViz.svelte       # Animated data flow
├── DiskIoPanel.svelte         # Disk activity
├── AssemblyView.svelte        # Hot instructions
└── CpuPipelineViz.svelte      # Pipeline stages
```

## Performance Targets
- Cache topology: Static, load once
- Performance counters: 1 second polling
- Flow animation: 60fps via requestAnimationFrame
- Assembly sampling: On-demand (expensive)
