# Tooling Landscape Analysis

Evaluating existing tools vs. direct interface access for the System Observatory.

## Categories

1. **Profiling** - Performance analysis, hotspots, bottlenecks
2. **Debugging** - Step execution, memory inspection, breakpoints
3. **Tracing** - Event capture, system call monitoring
4. **Reverse Engineering** - Binary analysis, instrumentation
5. **Observability** - Metrics, logs, distributed tracing

---

## Profiling Tools

### Intel VTune Profiler

| Aspect | Details |
|--------|---------|
| **What it does** | CPU profiling, cache/memory analysis, threading analysis |
| **Data access** | Hardware PMCs, ETW, sampling, instrumentation |
| **Output** | GUI with flame graphs, timelines, hotspot tables |
| **Strengths** | Most accurate cache data, understands Intel microarchitecture |
| **Weaknesses** | Intel-focused, heavy, complex UI, not embeddable |
| **License** | Free (standalone), was $899 |

**Leverage potential:**
- Could parse VTune's exported data (CSV, database)
- Could launch VTune with pre-configured analysis
- VTune has CLI (`vtune -collect`) for scripted capture

```bash
# Capture memory access analysis for 5 seconds
vtune -collect memory-access -duration 5 -target-pid 1234 -result-dir ./vtune_result

# Export to CSV
vtune -report hotspots -result-dir ./vtune_result -format csv -report-output hotspots.csv
```

**Verdict:** Best for accurate cache data. Could import results rather than recreate.

---

### AMD μProf

| Aspect | Details |
|--------|---------|
| **What it does** | CPU/GPU profiling, power analysis |
| **Data access** | AMD PMCs, IBS (Instruction-Based Sampling) |
| **Output** | GUI similar to VTune |
| **Strengths** | AMD-specific insights, free |
| **Weaknesses** | AMD only, less mature than VTune |

**Verdict:** Use for AMD systems, same integration approach as VTune.

---

### Windows Performance Analyzer (WPA)

| Aspect | Details |
|--------|---------|
| **What it does** | Visualize ETW traces |
| **Data access** | ETW traces (.etl files) |
| **Output** | GUI with customizable graphs, tables |
| **Strengths** | Understands all Windows internals, very powerful |
| **Weaknesses** | Steep learning curve, not scriptable, can't embed |

**Leverage potential:**
- WPR (Windows Performance Recorder) captures traces we could also read
- ETL file format is documented, could parse directly
- Could launch WPA with specific views pre-configured

```bash
# Capture with WPR
wpr -start CPU -start DiskIO -start FileIO
wpr -stop trace.etl

# Open in WPA with profile
wpa trace.etl -profile MyProfile.wpaProfile
```

**Verdict:** Great for complex analysis. We should capture same ETW data directly.

---

### perf (Linux)

| Aspect | Details |
|--------|---------|
| **What it does** | CPU performance counters, sampling, tracing |
| **Data access** | perf_event_open syscall, kernel PMU driver |
| **Output** | Text reports, perf.data files |
| **Strengths** | Low overhead, kernel-integrated, scriptable |
| **Weaknesses** | Linux only, text-based output |

```bash
# Cache miss analysis
perf stat -e cache-references,cache-misses,L1-dcache-load-misses ./program

# Record with call stacks
perf record -g -p 1234 sleep 5
perf report
```

**Verdict:** Excellent for WSL2 Linux processes. Could integrate via WSL commands.

---

### Cachegrind (Valgrind)

| Aspect | Details |
|--------|---------|
| **What it does** | Cache simulation, branch prediction simulation |
| **Data access** | Binary instrumentation (runs program under Valgrind) |
| **Output** | Detailed cache miss counts per line of code |
| **Strengths** | Exact cache behavior (simulated), no hardware needed |
| **Weaknesses** | 10-50x slowdown, simulation may differ from real CPU |

```bash
valgrind --tool=cachegrind ./program
cg_annotate cachegrind.out.12345
```

**Verdict:** Useful for educational simulation mode. Similar approach to our Phase 7.

---

## Debugging Tools

### WinDbg (Windows Debugger)

| Aspect | Details |
|--------|---------|
| **What it does** | Kernel and user-mode debugging, crash analysis |
| **Data access** | Debug API, kernel debugging, TTD |
| **Output** | Command-line interface, some GUI |
| **Strengths** | Complete access to everything, scriptable (JS, DX) |
| **Weaknesses** | Complex command syntax, steep learning curve |

**Leverage potential:**
- WinDbg has extensions API - could write custom extension
- TTD (Time Travel Debugging) records execution for replay
- DbgEng API is documented, could embed debugger

```javascript
// WinDbg JavaScript extension
function analyzeCache(addr) {
    let line = host.memory.readMemoryValues(addr, 64, 1);
    // ... analyze cache line
}
```

**Verdict:** DbgEng API could provide deep inspection. TTD is essentially what we want for replay.

---

### x64dbg

| Aspect | Details |
|--------|---------|
| **What it does** | User-mode debugger for Windows |
| **Data access** | Debug API |
| **Output** | GUI with disassembly, registers, memory |
| **Strengths** | Open source, plugin system, good UI |
| **Weaknesses** | User-mode only, no kernel access |

**Leverage potential:**
- Open source - could study implementation
- Plugin system - could write Observatory plugin
- Has trace functionality built in

**Verdict:** Good reference for UI patterns. Could launch with process attached.

---

### GDB

| Aspect | Details |
|--------|---------|
| **What it does** | Debugger for Linux/Unix |
| **Data access** | ptrace, /proc, debug symbols |
| **Output** | Command line, TUI, MI (machine interface) |
| **Strengths** | Scriptable (Python), MI for tool integration |
| **Weaknesses** | Linux only (but works in WSL) |

**Leverage potential:**
- GDB/MI protocol well-documented for integration
- Python scripting for custom analysis
- Could connect to gdbserver for remote debugging

```python
# GDB Python script
class CacheAnalyzer(gdb.Command):
    def invoke(self, arg, from_tty):
        # Read memory, analyze patterns
        mem = gdb.selected_inferior().read_memory(addr, 64)
```

**Verdict:** Use for WSL process debugging via MI protocol.

---

## Tracing Tools

### ETW (Event Tracing for Windows)

| Aspect | Details |
|--------|---------|
| **What it does** | High-performance kernel/user event tracing |
| **Data access** | Kernel providers, user providers |
| **Output** | ETL files, real-time consumption |
| **Strengths** | Low overhead, comprehensive, built into Windows |
| **Weaknesses** | Complex API, provider discovery is hard |

**Key providers for Observatory:**
```
Microsoft-Windows-Kernel-Memory      # Page faults, working set
Microsoft-Windows-Kernel-Disk        # Disk I/O
Microsoft-Windows-Kernel-Process     # Process lifecycle
Microsoft-Windows-Kernel-Processor-Power  # CPU states
```

**Verdict:** THIS IS THE FOUNDATION. Direct ETW access is essential.

---

### Process Monitor (ProcMon)

| Aspect | Details |
|--------|---------|
| **What it does** | File, Registry, Network, Process monitoring |
| **Data access** | Filter driver (minifilter), ETW |
| **Output** | GUI with filterable event list |
| **Strengths** | Excellent UI, powerful filtering, free |
| **Weaknesses** | Can't embed, no API, focused on file/reg/net |

**Leverage potential:**
- Can export to CSV/XML for import
- Command-line mode: `procmon /BackingFile log.pml /Quiet`
- Could study UI patterns

**Verdict:** Great reference for event filtering UI. Capture same data via ETW.

---

### API Monitor

| Aspect | Details |
|--------|---------|
| **What it does** | Win32/COM API call tracing |
| **Data access** | IAT hooking, inline hooking |
| **Output** | GUI with call tree, parameters, return values |
| **Strengths** | Sees all API calls with full parameters |
| **Weaknesses** | Closed source, can't embed |

**Verdict:** Useful reference. Could achieve similar with Frida or Detours.

---

### strace / ltrace (Linux)

| Aspect | Details |
|--------|---------|
| **What it does** | System call / library call tracing |
| **Data access** | ptrace |
| **Output** | Text log of calls |
| **Strengths** | Simple, always available |
| **Weaknesses** | Performance overhead, Linux only |

```bash
strace -e trace=memory -p 1234  # Memory-related syscalls
ltrace -p 1234                   # Library calls
```

**Verdict:** Use via WSL for Linux processes.

---

## Reverse Engineering / Instrumentation

### Frida

| Aspect | Details |
|--------|---------|
| **What it does** | Dynamic instrumentation toolkit |
| **Data access** | Injects JavaScript engine, hooks functions |
| **Output** | Scriptable, real-time |
| **Strengths** | Cross-platform, scriptable, powerful |
| **Weaknesses** | Detected by anti-cheat, some overhead |

**Leverage potential:**
```javascript
// Frida script to trace memory accesses
Interceptor.attach(ptr("0x7ff612340000"), {
    onEnter: function(args) {
        console.log("Accessing: " + this.context.rax);
    }
});
```

Could use Frida as our instrumentation layer!

**Verdict:** STRONG CANDIDATE for process-level tracing. Scriptable, cross-platform.

---

### Intel Pin

| Aspect | Details |
|--------|---------|
| **What it does** | Binary instrumentation framework |
| **Data access** | JIT recompilation, intercepts every instruction |
| **Output** | Custom - whatever your Pintool outputs |
| **Strengths** | Complete visibility, accurate |
| **Weaknesses** | 2-10x slowdown, complex to write Pintools |

```cpp
// Pintool for memory access tracing
VOID RecordMemRead(VOID* ip, VOID* addr, UINT32 size) {
    fprintf(trace, "R %p %p %d\n", ip, addr, size);
}

VOID Instruction(INS ins, VOID* v) {
    if (INS_IsMemoryRead(ins)) {
        INS_InsertCall(ins, IPOINT_BEFORE, (AFUNPTR)RecordMemRead,
            IARG_INST_PTR, IARG_MEMORYREAD_EA, IARG_MEMORYREAD_SIZE, IARG_END);
    }
}
```

**Verdict:** Best for complete memory access tracing. Use for detailed capture.

---

### DynamoRIO

| Aspect | Details |
|--------|---------|
| **What it does** | Dynamic binary instrumentation (like Pin) |
| **Data access** | Runtime code transformation |
| **Output** | Custom |
| **Strengths** | Open source, actively maintained |
| **Weaknesses** | Similar overhead to Pin |

Comes with useful tools:
- `drcachesim` - Cache simulator (exactly what we want!)
- `drcov` - Code coverage
- `drltrace` - Library call tracing

```bash
# Run with cache simulator
drrun -t drcachesim -- ./program

# Output: Cache simulation results
```

**Verdict:** STRONG CANDIDATE. `drcachesim` is literally a cache simulator we could leverage.

---

### Ghidra / IDA Pro

| Aspect | Details |
|--------|---------|
| **What it does** | Static binary analysis, disassembly, decompilation |
| **Data access** | Binary parsing |
| **Output** | GUI with annotated disassembly |
| **Strengths** | Deep analysis, scripting |
| **Weaknesses** | Static (no runtime), complex |

**Leverage potential:**
- Could use Ghidra headless for symbol extraction
- Import function names for better trace annotation

**Verdict:** Useful for static analysis, not core to Observatory.

---

## Hardware Access

### Intel PCM (Performance Counter Monitor)

| Aspect | Details |
|--------|---------|
| **What it does** | Read CPU performance counters |
| **Data access** | MSRs via kernel driver |
| **Output** | CSV, real-time console |
| **Strengths** | Most accurate cache/memory data, open source |
| **Weaknesses** | Requires kernel driver install |

```bash
pcm --csv -i=1  # 1 second interval, CSV output

# Output:
# L3MISS, L2MISS, L3HIT, L2HIT, L3MPI, L2MPI, READ, WRITE, ...
```

**Verdict:** ESSENTIAL for accurate cache hit/miss data. Integrate via driver + parsing.

---

### CPUID

| Aspect | Details |
|--------|---------|
| **What it does** | Query CPU features, cache topology |
| **Data access** | CPUID instruction (user-mode accessible) |
| **Output** | Raw register values |
| **Strengths** | No privileges needed, accurate topology |
| **Weaknesses** | Static info only, complex to parse |

**Verdict:** Already using via `raw-cpuid`. Foundation for cache structure.

---

### MSR (Model-Specific Registers)

| Aspect | Details |
|--------|---------|
| **What it does** | Read/write CPU configuration and counters |
| **Data access** | Requires ring 0 (kernel driver) |
| **Output** | Raw values |
| **Strengths** | Direct access to everything |
| **Weaknesses** | Needs driver, CPU-specific, can crash system |

**Verdict:** Intel PCM wraps this. Use PCM rather than direct MSR access.

---

## Comparison Matrix

| Tool | Cache Data | Memory Trace | Disk I/O | Embeddable | Real-time | Complexity |
|------|------------|--------------|----------|------------|-----------|------------|
| **VTune** | ★★★★★ | ★★★★ | ★★★ | ✗ Export only | ✗ | High |
| **WPA/WPR** | ★★★ | ★★★★★ | ★★★★★ | ✗ Launch only | ✓ | High |
| **perf** | ★★★★ | ★★★★ | ★★★ | ✓ Parse output | ✓ | Medium |
| **ETW Direct** | ★★★ | ★★★★★ | ★★★★★ | ✓ Full control | ✓ | High |
| **Frida** | ✗ | ★★★★ | ✗ | ✓ Scriptable | ✓ | Medium |
| **Intel Pin** | ★★★★★ | ★★★★★ | ✗ | ✓ Custom tool | ✗ Slow | High |
| **DynamoRIO** | ★★★★★ | ★★★★★ | ✗ | ✓ drcachesim | ✗ Slow | High |
| **Intel PCM** | ★★★★★ | ★★ | ✗ | ✓ Parse output | ✓ | Low |
| **ProcMon** | ✗ | ★★★ | ★★★★★ | ✗ Export only | ✓ | Low |

---

## Recommended Hybrid Approach

### Tier 1: Direct Integration (Build)

These provide core functionality, worth implementing directly:

| Component | Implementation | Reason |
|-----------|----------------|--------|
| **Cache topology** | `raw-cpuid` | Already done, no alternative needed |
| **ETW tracing** | Direct `windows-rs` | Full control, real-time, low overhead |
| **Memory inspection** | Debug API / `ReadProcessMemory` | Need direct access for inspector |
| **Working set** | `QueryWorkingSetEx` | Simple API, real-time |

### Tier 2: Tool Integration (Leverage)

These are better as integrations than reimplementations:

| Capability | Tool | Integration Method |
|------------|------|-------------------|
| **Accurate cache counters** | Intel PCM | Install driver, parse CSV output |
| **Full memory trace** | DynamoRIO `drcachesim` | Run process under DR, import results |
| **Function-level profiling** | VTune / perf | Launch with config, import results |
| **API call tracing** | Frida | Embed frida-core, write scripts |

### Tier 3: Launch Points (External)

Provide one-click launch to these for deep analysis:

| Tool | When to Use |
|------|-------------|
| **WinDbg** | Kernel debugging, crash analysis |
| **VTune** | Detailed microarchitecture analysis |
| **WPA** | Complex ETW analysis |
| **x64dbg** | User-mode debugging with GUI |

---

## Integration Architecture

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        System Observatory                                │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                          │
│  ┌─ Direct Implementation ────────────────────────────────────────────┐ │
│  │                                                                     │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐              │ │
│  │  │  raw-cpuid   │  │  ETW Direct  │  │  Debug API   │              │ │
│  │  │  (topology)  │  │  (tracing)   │  │  (inspect)   │              │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘              │ │
│  │                                                                     │ │
│  └─────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  ┌─ Tool Integration ─────────────────────────────────────────────────┐ │
│  │                                                                     │ │
│  │  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐              │ │
│  │  │  Intel PCM   │  │  DynamoRIO   │  │    Frida     │              │ │
│  │  │  (counters)  │  │  (cachesim)  │  │  (hooks)     │              │ │
│  │  │              │  │              │  │              │              │ │
│  │  │ ┌──────────┐ │  │ ┌──────────┐ │  │ ┌──────────┐ │              │ │
│  │  │ │ Adapter  │ │  │ │ Adapter  │ │  │ │ Adapter  │ │              │ │
│  │  │ │ (parse)  │ │  │ │ (import) │ │  │ │ (bridge) │ │              │ │
│  │  │ └──────────┘ │  │ └──────────┘ │  │ └──────────┘ │              │ │
│  │  └──────────────┘  └──────────────┘  └──────────────┘              │ │
│  │                                                                     │ │
│  └─────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
│  ┌─ External Launch ──────────────────────────────────────────────────┐ │
│  │                                                                     │ │
│  │  [Launch VTune]  [Launch WPA]  [Launch WinDbg]  [Launch x64dbg]    │ │
│  │                                                                     │ │
│  │  Pre-configured with:                                               │ │
│  │  - Target process attached                                          │ │
│  │  - Analysis type selected                                           │ │
│  │  - Filters applied                                                  │ │
│  │                                                                     │ │
│  └─────────────────────────────────────────────────────────────────────┘ │
│                                                                          │
└─────────────────────────────────────────────────────────────────────────┘
```

---

## Specific Integration Plans

### Intel PCM Integration

```rust
// Rust adapter for Intel PCM
pub struct PcmAdapter {
    pcm_path: PathBuf,
    process: Option<Child>,
}

impl PcmAdapter {
    pub fn start_monitoring(&mut self) -> Result<()> {
        self.process = Some(
            Command::new(&self.pcm_path)
                .args(["--csv", "-i=1"])
                .stdout(Stdio::piped())
                .spawn()?
        );
        Ok(())
    }

    pub fn read_counters(&mut self) -> Result<CacheCounters> {
        // Parse CSV line from stdout
        let line = self.read_line()?;
        parse_pcm_csv(&line)
    }
}

struct CacheCounters {
    l3_hit_ratio: f64,
    l2_hit_ratio: f64,
    l3_misses_per_sec: u64,
    memory_bandwidth_gbps: f64,
    // ...
}
```

### DynamoRIO Integration

```rust
// Run process under DynamoRIO cache simulator
pub async fn run_with_cachesim(
    executable: &Path,
    args: &[String],
    config: CacheSimConfig,
) -> Result<CacheSimResult> {
    let output_file = tempfile::NamedTempFile::new()?;

    let status = Command::new("drrun")
        .args(["-t", "drcachesim"])
        .args(["-LL_size", &config.l3_size.to_string()])
        .args(["-L2_size", &config.l2_size.to_string()])
        .args(["-L1D_size", &config.l1d_size.to_string()])
        .args(["-outfile", output_file.path().to_str().unwrap()])
        .arg("--")
        .arg(executable)
        .args(args)
        .status()?;

    parse_drcachesim_output(output_file.path())
}
```

### Frida Integration

```rust
// Frida integration for function hooking
pub struct FridaSession {
    session: frida::Session,
    script: frida::Script,
}

impl FridaSession {
    pub fn attach(pid: u32) -> Result<Self> {
        let frida = frida::Frida::obtain();
        let device = frida.get_local_device()?;
        let session = device.attach(pid)?;

        let script = session.create_script(MEMORY_TRACE_SCRIPT)?;
        script.load()?;

        Ok(Self { session, script })
    }

    pub fn on_memory_access(&self, callback: impl Fn(MemoryAccess)) {
        self.script.set_message_handler(move |msg| {
            if let Some(access) = parse_memory_access(msg) {
                callback(access);
            }
        });
    }
}

const MEMORY_TRACE_SCRIPT: &str = r#"
    // JavaScript that runs inside target process
    Interceptor.attach(targetFunction, {
        onEnter: function(args) {
            send({ type: 'memory', address: args[0].toInt32() });
        }
    });
"#;
```

---

## Trade-off Summary

| Approach | Pros | Cons |
|----------|------|------|
| **Direct ETW/API** | Full control, real-time, low overhead | More work, Windows-specific |
| **Intel PCM** | Most accurate cache data | Requires driver install |
| **DynamoRIO** | Complete memory trace | Slowdown (2-10x), offline only |
| **Frida** | Flexible, scriptable, cross-platform | Some overhead, detectable |
| **Tool Launch** | Leverage existing UI | No integration, context switch |

## Recommendation

**Phase 1-3:** Direct implementation (ETW, Debug API, raw-cpuid)
- Provides foundation
- Real-time capable
- Full control over visualization

**Phase 4+:** Add tool integrations
- Intel PCM for accurate counters
- DynamoRIO for educational cache simulation
- Frida for advanced hooking

**Always:** Provide launch points to external tools
- VTune, WPA for deep analysis
- WinDbg for debugging
- Let experts use expert tools

This hybrid approach gives us:
1. **Real-time visualization** from direct implementation
2. **Accurate data** from PCM integration
3. **Educational simulation** from DynamoRIO
4. **Escape hatches** to powerful external tools
