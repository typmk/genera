# System Observatory

> A live, interactive introspection tool for the physical machine - like SLIME/Swank but for hardware.

## Vision

Transform the Task Manager's Explore tab into a **System Observatory** - showing the actual structure and behavior of CPU, cache, memory, and storage. Not progress bars and percentages, but the real architecture: pipeline stages, cache sets and ways, RAM banks and rows, disk pages and blocks.

**Inspirations:**
- **Shenzhen I/O** - See actual signals, timing, constraints. Learn by seeing the real machine.
- **Smalltalk** - Everything inspectable. Click anything, see its internals.
- **SLIME/Swank/SBCL** - Live introspection from inside the running system. Infinite drill-down.

## Core Principles

| Principle | Meaning |
|-----------|---------|
| **Transparency** | Show actual structure, not metaphors. Cache is sets/ways, not a progress bar. |
| **Introspection** | Click anything to drill infinitely deep. Like SLIME's inspector. |
| **Liveness** | Real data from the running system, not static diagrams. |
| **Time Control** | Slow down nanoseconds to seconds. Pause, scrub, replay. |
| **Concrete** | Show bytes, addresses, signals - the actual data, not abstractions. |

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          System Observatory                              │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                         Mode Selector                               │ │
│  │   [Live]  Real-time, sampled data from running system              │ │
│  │   [Trace] Recorded session, full detail, scrubbable                │ │
│  │   [Sim]   Educational simulation, adjustable speed                 │ │
│  └────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                      Component Views                                │ │
│  │                                                                     │ │
│  │   ┌─────────────┐    ┌─────────────┐    ┌─────────────┐            │ │
│  │   │     CPU     │    │    Cache    │    │     RAM     │            │ │
│  │   │  Pipeline   │───▶│  Sets/Ways  │───▶│ Banks/Rows  │            │ │
│  │   │  Registers  │    │  Tag/Data   │    │ Row Buffer  │            │ │
│  │   └─────────────┘    └─────────────┘    └──────┬──────┘            │ │
│  │                                                 │                   │ │
│  │                                                 ▼                   │ │
│  │                                          ┌─────────────┐            │ │
│  │                                          │    Disk     │            │ │
│  │                                          │ Pages/Blocks│            │ │
│  │                                          │ Queue/Dies  │            │ │
│  │                                          └─────────────┘            │ │
│  └────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                    Inspector (SLIME-style)                          │ │
│  │  Click any element → see internals → drill deeper → modify/watch   │ │
│  └────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  ┌────────────────────────────────────────────────────────────────────┐ │
│  │                    Timeline / Scrubber                              │ │
│  │  [⏪] [▶] [⏩]  Speed: 0.001x   ═══════●═══════════════  +2.4ms     │ │
│  └────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

## Component Specifications

### 1. CPU View

**What it shows:**
- Pipeline stages: Fetch → Decode → Execute → Memory → Writeback
- Instructions flowing through stages (animated)
- Pipeline stalls and bubbles
- Execution units: ALU, FPU, Load/Store, Branch
- Registers with live values
- Branch predictor state and accuracy

**Data sources:**
- ETW CPU sampling for instruction flow
- Performance counters for IPC, stalls, branch mispredictions
- Debug API for register values (attached process)

**Visualization:**
```
┌──────┐   ┌──────┐   ┌──────┐   ┌──────┐   ┌──────┐
│FETCH │──▶│DECODE│──▶│ EXEC │──▶│ MEM  │──▶│  WB  │
│ mov  │   │ add  │   │ sub  │   │ load │   │ done │
│ rax  │   │ rbx  │   │ rcx  │   │[rdx] │   │      │
└──────┘   └──────┘   └──────┘   └──────┘   └──────┘
     │                    │
     │    ┌───────────────┘
     │    ▼
┌─────────────────────┐   ┌─────────────────────┐
│ Registers           │   │ Execution Units     │
│ RAX: 0x7ff612340008 │   │ ALU 0: ██░░ active  │
│ RBX: 0x0000000008   │   │ ALU 1: ░░░░ idle    │
│ RCX: 0x0000000000   │   │ FPU:   ░░░░ idle    │
│ ...                 │   │ Load:  ████ stalled │
└─────────────────────┘   └─────────────────────┘
```

### 2. Cache View

**What it shows:**
- L1D, L1I, L2, L3 as actual set-associative structures
- Each set with its N ways
- Tag, data, state (MESI) for each line
- LRU state within each set
- Hit/miss visualization on access
- Eviction animation

**Data sources:**
- `raw-cpuid` for topology (size, associativity, line size)
- Intel PCM for hit/miss rates
- Simulation for detailed line tracking
- ETW memory access for addresses

**Visualization:**
```
L1 Data Cache (32KB, 8-way, 64B lines)
Set 0x34:
┌────────┬────────┬────────┬────────┬────────┬────────┬────────┬────────┐
│ Way 0  │ Way 1  │ Way 2  │ Way 3  │ Way 4  │ Way 5  │ Way 6  │ Way 7  │
│ Tag:   │ Tag:   │ Tag:   │ Tag:   │ Tag:   │ Tag:   │ Tag:   │ Tag:   │
│ 7ff612 │ 400100 │ (empty)│ (empty)│ (empty)│ (empty)│ (empty)│ (empty)│
│ MESI:M │ MESI:S │        │        │        │        │        │        │
│ LRU: 0 │ LRU: 1 │        │        │        │        │        │        │
└────────┴────────┴────────┴────────┴────────┴────────┴────────┴────────┘
         ▲
         └── Click to inspect: see 64 bytes, decode as instructions/data
```

### 3. RAM View

**What it shows:**
- Physical structure: channels, DIMMs, ranks, banks
- Row/column addressing
- Row buffer state (which row is "open" per bank)
- Access patterns: sequential vs random
- Timing: row activation, column select, data transfer
- Memory controller queue

**Data sources:**
- SMBIOS for physical topology
- Performance counters for bandwidth
- ETW for access patterns
- Simulation for row buffer behavior

**Visualization:**
```
Channel 0, DIMM 0, Rank 0
┌─ Bank 0 ─┐  ┌─ Bank 1 ─┐  ┌─ Bank 2 ─┐  ┌─ Bank 3 ─┐
│ ░░░░░░░░ │  │ ░░░░░░░░ │  │ ████████ │  │ ░░░░░░░░ │
│ ░░░░░░░░ │  │ ░░░░░░░░ │  │ ░░░░░░░░ │  │ ░░░░░░░░ │
│ ░░░░░░░░ │  │ ░░░░░░░░ │  │ ░░░░░░░░ │  │ ░░░░░░░░ │
└──────────┘  └──────────┘  └──────────┘  └──────────┘
                              Row 0x1234 open
                              ┌────────────────────────┐
                              │ Row Buffer (8KB)       │
                              │ ████████████████████░░ │
                              └────────────────────────┘

Timing: Row Hit = 15ns | Row Miss = 50ns | Row Conflict = 65ns
```

### 4. Disk View

**What it shows:**

**For HDD:**
- Physical platters (animated spinning)
- Head position and seeking
- Track/sector layout
- Rotational latency visualization

**For SSD/NVMe:**
- Dies, planes, blocks, pages
- Flash Translation Layer mapping
- Write amplification
- Command queue depth
- Parallel operations across dies

**Data sources:**
- ETW disk provider for I/O operations
- S.M.A.R.T. for drive info
- `DeviceIoControl` for queue depth

**Visualization (NVMe):**
```
NVMe SSD: Samsung 970 EVO Plus
┌── Die 0 ──┐  ┌── Die 1 ──┐  ┌── Die 2 ──┐  ┌── Die 3 ──┐
│ ┌──┬──┬──┐│  │ ┌──┬──┬──┐│  │ ┌──┬──┬──┐│  │ ┌──┬──┬──┐│
│ │▓▓│░░│░░││  │ │░░│░░│░░││  │ │░░│░░│░░││  │ │░░│▓▓│░░││
│ ├──┼──┼──┤│  │ ├──┼──┼──┤│  │ ├──┼──┼──┤│  │ ├──┼──┼──┤│
│ │░░│░░│░░││  │ │░░│░░│░░││  │ │░░│░░│░░││  │ │░░│░░│░░││
│ └──┴──┴──┘│  │ └──┴──┴──┘│  │ └──┴──┴──┘│  │ └──┴──┴──┘│
└───────────┘  └───────────┘  └───────────┘  └───────────┘
     ▲ Reading                                    ▲ Reading

Command Queue (Depth: 32/64):
[Read 4K] [Read 4K] [Write 16K] [Read 4K] [Trim] ...
    ✓         ◐         ○          ○        ○
```

### 5. Inspector Panel

**SLIME-style infinite drill-down:**

```
┌─ Inspector: L1D Cache Line ────────────────────────────────────────┐
│                                                                     │
│ Address: 0x7ff6`12340040                                           │
│ ├─ Tag:    0x7ff612                                                │
│ ├─ Index:  0x34 (set 52 of 64)                                     │
│ └─ Offset: 0x00                                                    │
│                                                                     │
│ State: Modified (M)                                                 │
│ LRU Position: 0 (least recently used - next eviction victim)       │
│ Last Access: 42ns ago @ cycle 1,284,847,150                        │
│                                                                     │
│ ┌─ Data (64 bytes) ────────────────────────────────────────────┐   │
│ │ 00: 48 8b 43 08  48 8b 08 48  │ H.C.H..H  mov rax,[rbx+8]    │   │
│ │ 08: 8b 91 00 04  00 00 48 85  │ ......H.  mov edx,[rcx+0x400]│   │
│ │ 10: c0 74 0a 48  8b 00 48 89  │ .t.H..H.  test rax,rax      │   │
│ │ 18: 44 24 08 eb  05 48 c7 44  │ D$...H.D  jz +0xa           │   │
│ │ 20: ...                                                      │   │
│ └──────────────────────────────────────────────────────────────┘   │
│                                                                     │
│ ┌─ Actions ────────────────────────────────────────────────────┐   │
│ │ [View in RAM]  [Track Accesses]  [Set Watchpoint]  [Decode]  │   │
│ └──────────────────────────────────────────────────────────────┘   │
│                                                                     │
│ ┌─ Containing Structures ──────────────────────────────────────┐   │
│ │ └─ L1D Cache                                                 │   │
│ │    └─ Set 0x34                                               │   │
│ │       └─ Way 0 ← you are here                                │   │
│ │          └─ Line 0x7ff612340040                              │   │
│ └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

**Inspectable objects:**
- Cache line → bytes, decoded instructions, tag, state
- RAM page → virtual/physical mapping, page table entry, contents
- Disk block → LBA, file mapping, raw data
- Register → value, last writer, dependents
- Instruction → encoding, operands, latency, dependencies

### 6. Timeline / Scrubber

**Time control for trace playback:**

```
┌─ Timeline ──────────────────────────────────────────────────────────────┐
│                                                                          │
│  Process: chrome.exe (PID 1234)           Duration: 2.4ms captured      │
│                                                                          │
│  [⏮] [⏪] [▶ Play] [⏩] [⏭]    Speed: [0.001x ▾]    [Loop] [Export]     │
│                                                                          │
│  ═══════════════════════════●══════════════════════════════════════════ │
│  0.0ms                    1.2ms                                   2.4ms │
│                                                                          │
│  Event lanes:                                                            │
│  CPU    │▮│ │▮▮│  │▮│▮▮▮│    │▮▮│   │▮│    │▮▮▮▮│   │▮│               │
│  L1     │ │▮│  │▮▮│ │  │▮│▮▮▮│  │▮▮▮│ │▮▮▮▮│    │▮▮▮│ │▮│             │
│  L2     │ │ │  │  │▮│  │ │   │▮▮│   │▮│    │    │   │▮│ │             │
│  L3     │ │ │  │  │ │  │ │   │  │   │ │    │▮▮▮▮│   │ │ │             │
│  RAM    │ │ │  │  │ │  │ │   │  │▮▮▮│ │    │    │   │ │ │▮▮           │
│  Disk   │ │ │  │  │ │  │ │   │  │   │ │    │    │▮▮▮│ │ │             │
│                                                                          │
│  Current event: L2 cache miss @ 0x7ff612340040 → fetching from L3       │
│                                                                          │
└──────────────────────────────────────────────────────────────────────────┘
```

## Data Layer

### Trace Event Format

```rust
/// Compact binary trace event (32 bytes typical)
#[repr(C)]
struct TraceEvent {
    /// Nanosecond timestamp (relative to trace start)
    timestamp_ns: u64,

    /// Event type discriminant
    event_type: EventType,

    /// Process/thread context
    pid: u32,
    tid: u32,

    /// Event-specific payload
    payload: EventPayload,
}

#[repr(u8)]
enum EventType {
    // CPU events
    InstructionSample = 0x01,
    ContextSwitch = 0x02,
    BranchMispredict = 0x03,

    // Memory events
    PageFault = 0x10,
    PageFaultHard = 0x11,  // Required disk I/O
    WorkingSetChange = 0x12,

    // Cache events (simulated or from PMC)
    CacheAccess = 0x20,
    CacheMiss = 0x21,
    CacheEviction = 0x22,

    // Disk events
    DiskRead = 0x30,
    DiskWrite = 0x31,
    DiskFlush = 0x32,
}

#[repr(C)]
union EventPayload {
    page_fault: PageFaultPayload,
    cache_access: CacheAccessPayload,
    disk_io: DiskIoPayload,
    instruction: InstructionPayload,
}

struct PageFaultPayload {
    virtual_address: u64,
    is_write: bool,
    is_hard: bool,  // Required disk I/O
}

struct CacheAccessPayload {
    address: u64,
    level: u8,      // 1, 2, or 3
    hit: bool,
    set: u16,
    way: u8,
}

struct DiskIoPayload {
    offset_lba: u64,
    size_bytes: u32,
    latency_ns: u32,
    is_write: bool,
    queue_depth: u8,
}
```

### Trace Storage

```rust
/// Memory-mapped trace file for efficient random access
struct TraceFile {
    /// Header with metadata
    header: TraceHeader,

    /// Index for fast time-based lookup
    /// Maps time ranges to byte offsets
    time_index: BTreeMap<u64, u64>,

    /// Memory-mapped event data
    events: Mmap,
}

struct TraceHeader {
    magic: [u8; 4],           // "OBST" (Observatory Trace)
    version: u32,
    start_time: SystemTime,
    duration_ns: u64,
    event_count: u64,

    // System info at capture time
    cpu_model: String,
    cache_topology: CacheTopology,
    ram_size: u64,
    disk_info: Vec<DiskInfo>,
}
```

### Query API

```rust
/// Query interface for trace data
trait TraceQuery {
    /// Get events in time range
    fn query_time_range(
        &self,
        start_ns: u64,
        end_ns: u64,
    ) -> impl Iterator<Item = TraceEvent>;

    /// Get events for specific process
    fn query_process(
        &self,
        pid: u32,
        time_range: Option<Range<u64>>,
    ) -> impl Iterator<Item = TraceEvent>;

    /// Get events by type
    fn query_type(
        &self,
        event_type: EventType,
        time_range: Option<Range<u64>>,
    ) -> impl Iterator<Item = TraceEvent>;

    /// Get cache state at specific time
    fn cache_state_at(&self, time_ns: u64) -> CacheState;
}
```

## Frontend Components

### File Structure

```
src/lib/observatory/
├── Observatory.svelte          # Main container, mode switching
├── components/
│   ├── CpuView.svelte          # Pipeline, registers, execution units
│   ├── CacheView.svelte        # Set/way structure, hierarchical
│   ├── RamView.svelte          # Banks, rows, row buffer
│   ├── DiskView.svelte         # HDD platters or SSD dies
│   ├── DataFlow.svelte         # Animated connections
│   └── Inspector.svelte        # SLIME-style drill-down
├── timeline/
│   ├── Timeline.svelte         # Main timeline container
│   ├── Scrubber.svelte         # Time position control
│   ├── EventLanes.svelte       # Per-component event tracks
│   └── TimeScale.svelte        # Zoom and pan
├── simulation/
│   ├── CacheSimulator.ts       # LRU cache simulation
│   ├── RamSimulator.ts         # Row buffer simulation
│   └── TracePlayer.ts          # Replay logic
├── stores/
│   ├── trace.svelte.ts         # Trace data store
│   ├── playback.svelte.ts      # Playback state
│   ├── selection.svelte.ts     # Selected/inspected object
│   └── view.svelte.ts          # View options, zoom level
└── types/
    └── observatory.ts          # TypeScript types
```

### Rendering Strategy

| Component | Technology | Reason |
|-----------|------------|--------|
| Cache structure | SVG | Clickable, accessible, scales well |
| Data flow animation | Canvas | Smooth 60fps particle animation |
| Timeline | Canvas + SVG | Dense data (Canvas), controls (SVG) |
| Hex viewer | DOM table | Text selection, accessibility |
| RAM heatmap | WebGL (optional) | Millions of cells if needed |

### Interaction Patterns

```typescript
// Click-to-inspect pattern
interface Inspectable {
    id: string;
    type: 'cache-line' | 'ram-page' | 'disk-block' | 'register' | ...;
    summary: string;

    // Drill-down data
    fields: InspectorField[];
    children?: () => Promise<Inspectable[]>;

    // Actions
    actions: InspectorAction[];
}

interface InspectorField {
    name: string;
    value: string | number | Inspectable;
    format?: 'hex' | 'decimal' | 'binary' | 'address';
    editable?: boolean;  // For simulation mode
}

interface InspectorAction {
    label: string;
    icon: string;
    action: () => void;
}
```

## Backend Commands

### Tauri Command Interface

```rust
// === Topology (static) ===

#[tauri::command]
async fn get_cpu_topology() -> Result<CpuTopology, String>

#[tauri::command]
async fn get_cache_topology() -> Result<CacheTopology, String>

#[tauri::command]
async fn get_ram_topology() -> Result<RamTopology, String>

#[tauri::command]
async fn get_disk_info() -> Result<Vec<DiskInfo>, String>


// === Live Data ===

#[tauri::command]
async fn get_cpu_state() -> Result<CpuState, String>

#[tauri::command]
async fn get_cache_stats() -> Result<CacheStats, String>

#[tauri::command]
async fn get_memory_stats() -> Result<MemoryStats, String>

#[tauri::command]
async fn get_disk_stats() -> Result<Vec<DiskStats>, String>


// === Tracing ===

#[tauri::command]
async fn start_trace(config: TraceConfig) -> Result<TraceId, String>

#[tauri::command]
async fn stop_trace(id: TraceId) -> Result<TraceSummary, String>

#[tauri::command]
async fn get_trace_events(
    id: TraceId,
    start_ns: u64,
    end_ns: u64,
    filters: EventFilters,
) -> Result<Vec<TraceEvent>, String>


// === Inspection ===

#[tauri::command]
async fn inspect_address(address: u64, pid: u32) -> Result<MemoryInspection, String>

#[tauri::command]
async fn read_memory(pid: u32, address: u64, size: u32) -> Result<Vec<u8>, String>

#[tauri::command]
async fn get_cache_line(level: u8, set: u32, way: u8) -> Result<CacheLine, String>


// === Process Attachment ===

#[tauri::command]
async fn attach_process(pid: u32) -> Result<AttachInfo, String>

#[tauri::command]
async fn detach_process(pid: u32) -> Result<(), String>

#[tauri::command]
async fn set_watchpoint(pid: u32, address: u64, size: u32) -> Result<WatchpointId, String>
```

## Implementation Phases

### Phase 1: Foundation (Week 1-2)

**Goal:** Basic trace capture and playback infrastructure.

- [ ] ETW session management (start/stop trace)
- [ ] Ring buffer for event capture
- [ ] Binary trace file format
- [ ] Basic Tauri commands for trace control
- [ ] Minimal timeline UI with scrubber

**Deliverable:** Can capture 5 seconds of memory events, save to file, scrub through.

### Phase 2: Structure Views (Week 3-4)

**Goal:** Replace progress-bar visualizations with structural views.

- [ ] Cache set/way visualization (SVG, clickable)
- [ ] Basic inspector panel (click cache line → see data)
- [ ] RAM bank visualization (simplified)
- [ ] CPU pipeline placeholder (static diagram)

**Deliverable:** Explore tab shows actual cache structure, can inspect individual lines.

### Phase 3: Data Flow (Week 5-6)

**Goal:** Connect components with animated data flow.

- [ ] Data flow canvas layer
- [ ] Particle animation (address → cache → RAM)
- [ ] Access pattern visualization
- [ ] Latency indication (visual distance = time)

**Deliverable:** See data moving through the memory hierarchy in real-time.

### Phase 4: Full Tracing (Week 7-8)

**Goal:** Complete trace capture for attached process.

- [ ] ETW memory provider integration
- [ ] ETW disk provider integration
- [ ] Per-process filtering
- [ ] Event correlation (memory access → cache behavior)

**Deliverable:** Attach to Chrome, capture 1 second, see all memory/disk events.

### Phase 5: Timeline & Playback (Week 9-10)

**Goal:** Full timeline with multi-lane event visualization.

- [ ] Event lanes (CPU, L1, L2, L3, RAM, Disk)
- [ ] Zoom and pan
- [ ] Event markers and labels
- [ ] Synchronized playback across all views

**Deliverable:** Scrub through trace, all views update to show state at that time.

### Phase 6: Deep Inspection (Week 11-12)

**Goal:** SLIME-style infinite drill-down.

- [ ] Recursive inspector with lazy loading
- [ ] Memory hex viewer with live updates
- [ ] Decoded instructions overlay
- [ ] Cross-references (this line → accessed by these instructions)

**Deliverable:** Click anything, drill arbitrarily deep, see all relationships.

### Phase 7: Simulation Mode (Week 13-14)

**Goal:** Educational mode with simulated hardware.

- [ ] Cache simulator (configurable size, associativity)
- [ ] Step-by-step execution
- [ ] "Why slow?" explanations
- [ ] Guided scenarios (cache thrashing, false sharing, etc.)

**Deliverable:** Load a memory access pattern, step through, see cache behavior.

### Phase 8: Polish & Performance (Week 15-16)

**Goal:** Production quality.

- [ ] Performance optimization (large traces)
- [ ] Keyboard navigation
- [ ] Export/share traces
- [ ] Documentation and help

**Deliverable:** Ship-ready feature.

## Technical Challenges

### Challenge 1: ETW Complexity

ETW is powerful but complex. Need to handle:
- Session management
- Provider configuration
- Event parsing
- High event rates (100K+ events/sec)

**Mitigation:** Start with simple providers, use `windows-rs` ETW bindings, sample if needed.

### Challenge 2: Cache State Reconstruction

CPU doesn't expose actual cache contents. Options:
- Simulation based on access pattern
- Intel PCM for aggregate stats
- Intel Processor Trace for instruction flow

**Mitigation:** Use simulation with validation against real counters.

### Challenge 3: Performance at Scale

Large traces (millions of events) need:
- Efficient storage (memory-mapped files)
- Indexed queries (time-based index)
- Virtualized rendering (only render visible)

**Mitigation:** Design for scale from start, use appropriate data structures.

### Challenge 4: Cross-Component Correlation

Linking events across layers:
- This page fault → caused by this instruction → filled this cache line

**Mitigation:** Capture timestamps with high precision, use address matching.

## Success Criteria

1. **Educational:** Someone can watch a memory access traverse the hierarchy and understand *why* it took the time it did.

2. **Debugging:** Developer can identify "my app is slow because of L3 cache misses" in under 1 minute.

3. **Accurate:** Visualization matches real hardware behavior (validated against PMCs).

4. **Performant:** Handles 1M event traces smoothly, 60fps animation.

5. **Explorable:** Any visible element can be inspected, drilled into.

## Open Questions

1. **Scope:** Include GPU memory hierarchy? Network stack as another "storage tier"?

2. **Platform:** Windows-first, but design for Linux compatibility?

3. **Sharing:** Export traces for sharing? Compare traces side-by-side?

4. **Automation:** Scripted analysis? Detect anti-patterns automatically?

## References

- Intel 64 and IA-32 Architectures Optimization Reference Manual
- What Every Programmer Should Know About Memory (Drepper)
- ETW documentation (Microsoft)
- Intel Performance Counter Monitor (PCM) source
- Shenzhen I/O (game design inspiration)
- Smalltalk-80 Inspector (interaction model)
- SLIME/Swank (introspection architecture)
