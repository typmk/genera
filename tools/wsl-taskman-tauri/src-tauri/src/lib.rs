use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::Arc;
use std::time::Instant;
use tauri::{
    menu::{Menu, MenuItem},
    tray::{MouseButton, MouseButtonState, TrayIcon, TrayIconBuilder, TrayIconEvent},
    Emitter, Manager, State,
};

#[cfg(windows)]
use std::os::windows::process::CommandExt;
#[cfg(windows)]
use window_vibrancy::apply_mica;

// Windows API imports for fast process enumeration
#[cfg(windows)]
use windows::{
    Win32::Foundation::CloseHandle,
    Win32::System::Diagnostics::ToolHelp::{
        CreateToolhelp32Snapshot, Process32First, Process32Next, PROCESSENTRY32,
        TH32CS_SNAPPROCESS,
    },
    Win32::System::ProcessStatus::{K32GetProcessMemoryInfo, PROCESS_MEMORY_COUNTERS},
    Win32::System::SystemInformation::{GlobalMemoryStatusEx, MEMORYSTATUSEX},
    Win32::System::Threading::{
        OpenProcess, GetPriorityClass, SetPriorityClass, GetProcessTimes,
        GetProcessIoCounters, IO_COUNTERS,
        PROCESS_QUERY_INFORMATION, PROCESS_VM_READ,
        ABOVE_NORMAL_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
        IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS,
    },
};

// Native Windows memory info - no PowerShell overhead
#[cfg(windows)]
fn get_native_memory_info() -> Option<MemoryInfo> {
    unsafe {
        let mut mem_status = MEMORYSTATUSEX {
            dwLength: std::mem::size_of::<MEMORYSTATUSEX>() as u32,
            ..Default::default()
        };
        if GlobalMemoryStatusEx(&mut mem_status).is_ok() {
            let total_mb = (mem_status.ullTotalPhys / 1024 / 1024) as u64;
            let available_mb = (mem_status.ullAvailPhys / 1024 / 1024) as u64;
            let used_mb = total_mb.saturating_sub(available_mb);
            let percent = if total_mb > 0 {
                used_mb as f32 / total_mb as f32 * 100.0
            } else {
                0.0
            };
            Some(MemoryInfo { total_mb, used_mb, available_mb, percent })
        } else {
            None
        }
    }
}

#[cfg(windows)]
const CREATE_NO_WINDOW: u32 = 0x08000000;

// Cached system data for instant UI responses
#[derive(Clone, Debug, Default)]
pub struct CachedData {
    pub system_tree: Option<SystemTreeNode>,
    pub last_update: Option<Instant>,
    pub is_updating: bool,
}

// Persistent WSL shell - keeps one bash process alive for efficient polling
pub struct WslShell {
    _child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    _distro: String,
}

impl WslShell {
    fn new(distro: Option<&str>) -> Result<Self, String> {
        let mut cmd = Command::new("wsl");

        // Use specific distro if provided
        if let Some(d) = distro {
            cmd.args(["-d", d, "-e", "bash", "--norc", "--noprofile"]);
        } else {
            cmd.args(["-e", "bash", "--norc", "--noprofile"]);
        }

        cmd.stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null());

        #[cfg(windows)]
        cmd.creation_flags(CREATE_NO_WINDOW);

        let mut child = cmd.spawn().map_err(|e| format!("Failed to spawn WSL: {}", e))?;

        let stdin = child.stdin.take().ok_or("Failed to get stdin")?;
        let stdout = child.stdout.take().ok_or("Failed to get stdout")?;

        Ok(Self {
            _child: child,
            stdin,
            stdout: BufReader::new(stdout),
            _distro: distro.unwrap_or("default").to_string(),
        })
    }

    fn run(&mut self, command: &str) -> Result<String, String> {
        // Use a unique end marker to detect command completion
        let marker = format!("__END_MARKER_{}__", std::process::id());
        let full_cmd = format!("{}\necho '{}'\n", command, marker);

        self.stdin
            .write_all(full_cmd.as_bytes())
            .map_err(|e| format!("Write failed: {}", e))?;
        self.stdin.flush().map_err(|e| format!("Flush failed: {}", e))?;

        let mut output = String::new();
        let mut line = String::new();

        loop {
            line.clear();
            match self.stdout.read_line(&mut line) {
                Ok(0) => break, // EOF
                Ok(_) => {
                    if line.trim() == marker {
                        break;
                    }
                    output.push_str(&line);
                }
                Err(e) => return Err(format!("Read failed: {}", e)),
            }
        }

        Ok(output)
    }
}

// Global state
pub struct TrayState(pub Arc<Mutex<Option<TrayIcon>>>);
pub struct ShellState(pub Arc<Mutex<Option<WslShell>>>);
pub struct DistroState(pub Arc<Mutex<String>>); // Current distro name
pub struct CacheState(pub Arc<Mutex<CachedData>>); // Cached system data for instant UI

// ============================================================================
// Data Types
// ============================================================================

// System source types - WSL distros, Windows, VMs, remote hosts
#[derive(Clone, Debug, Serialize, Deserialize, PartialEq)]
#[serde(tag = "type")]
pub enum SystemSource {
    #[serde(rename = "wsl")]
    Wsl { distro: String, is_default: bool, is_running: bool },
    #[serde(rename = "windows")]
    Windows,
    #[serde(rename = "all")]
    All, // Combined view of all sources
}

// WSL distro info (for backward compat)
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WslDistro {
    pub name: String,
    pub is_default: bool,
    pub is_running: bool,
}

// Available system source for UI
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct AvailableSource {
    pub id: String,
    pub name: String,
    pub source_type: String,
    pub is_available: bool,
    pub icon: String,
}

// Hierarchical system tree node
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SystemTreeNode {
    pub id: String,
    pub name: String,
    pub node_type: String,  // "host", "vm", "wsl", "user", "process"
    pub icon: String,
    pub parent_id: Option<String>,
    pub host_pid: Option<u32>,  // For VMs: vmmem PID on host
    pub depth: u32,
    pub is_expanded: bool,
    pub child_count: usize,

    // Stats (aggregated for groups)
    pub cpu_percent: f32,
    pub memory_mb: f32,
    pub process_count: usize,

    // Process-specific fields (when node_type == "process")
    pub pid: Option<u32>,
    pub ppid: Option<u32>,
    pub user: Option<String>,
    pub state: Option<String>,
    pub command: Option<String>,

    // Children
    pub children: Vec<SystemTreeNode>,
}

impl SystemTreeNode {
    fn new_host(id: &str, name: &str, icon: &str) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            node_type: "host".to_string(),
            icon: icon.to_string(),
            parent_id: None,
            host_pid: None,
            depth: 0,
            is_expanded: true,
            child_count: 0,
            cpu_percent: 0.0,
            memory_mb: 0.0,
            process_count: 0,
            pid: None,
            ppid: None,
            user: None,
            state: None,
            command: None,
            children: Vec::new(),
        }
    }

    fn new_subsystem(id: &str, name: &str, node_type: &str, icon: &str, parent_id: &str, host_pid: Option<u32>) -> Self {
        Self {
            id: id.to_string(),
            name: name.to_string(),
            node_type: node_type.to_string(),
            icon: icon.to_string(),
            parent_id: Some(parent_id.to_string()),
            host_pid,
            depth: 1,
            is_expanded: true,
            child_count: 0,
            cpu_percent: 0.0,
            memory_mb: 0.0,
            process_count: 0,
            pid: None,
            ppid: None,
            user: None,
            state: None,
            command: None,
            children: Vec::new(),
        }
    }

    fn new_user(user: &str, parent_id: &str, depth: u32) -> Self {
        Self {
            id: format!("{}:user:{}", parent_id, user),
            name: user.to_string(),
            node_type: "user".to_string(),
            icon: "👤".to_string(),
            parent_id: Some(parent_id.to_string()),
            host_pid: None,
            depth,
            is_expanded: true,
            child_count: 0,
            cpu_percent: 0.0,
            memory_mb: 0.0,
            process_count: 0,
            pid: None,
            ppid: None,
            user: Some(user.to_string()),
            state: None,
            command: None,
            children: Vec::new(),
        }
    }

    fn from_process(p: &Process, parent_id: &str, depth: u32) -> Self {
        Self {
            id: format!("{}:proc:{}", parent_id, p.pid),
            name: p.short_name.clone(),
            node_type: "process".to_string(),
            icon: "".to_string(),
            parent_id: Some(parent_id.to_string()),
            host_pid: None,
            depth,
            is_expanded: false,
            child_count: 0,
            cpu_percent: p.cpu_percent,
            memory_mb: p.memory_mb,
            process_count: 1,
            pid: Some(p.pid),
            ppid: Some(p.ppid),
            user: Some(p.user.clone()),
            state: Some(p.state.clone()),
            command: Some(p.command.clone()),
            children: Vec::new(),
        }
    }

    fn aggregate_stats(&mut self) {
        let mut total_cpu = self.cpu_percent;
        let mut total_mem = self.memory_mb;
        let mut total_procs = if self.node_type == "process" { 1 } else { 0 };

        for child in &mut self.children {
            child.aggregate_stats();
            total_cpu += child.cpu_percent;
            total_mem += child.memory_mb;
            total_procs += child.process_count;
        }

        self.cpu_percent = total_cpu;
        self.memory_mb = total_mem;
        self.process_count = total_procs;
        self.child_count = self.children.len();
    }
}

// Unified process structure for all systems
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct Process {
    pub pid: u32,
    pub ppid: u32,           // Parent PID for tree view
    pub user: String,
    pub cpu_percent: f32,
    pub memory_mb: f32,
    pub memory_percent: f32,
    pub command: String,
    pub short_name: String,
    pub state: String,       // R=running, S=sleeping, D=disk, Z=zombie, T=stopped
    pub exe_path: String,    // Full executable path
    pub is_system: bool,     // True if system/daemon process
    pub source: String,      // "windows", "wsl:Ubuntu", "wsl:kali-linux", etc.
    // Extended metrics
    pub disk_read_bytes: u64,    // Total disk read bytes
    pub disk_write_bytes: u64,   // Total disk write bytes
    pub net_rx_bytes: u64,       // Network received bytes
    pub net_tx_bytes: u64,       // Network transmitted bytes
    pub threads: u32,            // Thread count
    pub handles: u32,            // Handle count (Windows)
    pub priority: i32,           // Process priority (-20 to 19 on Linux, 0-31 on Windows)
    pub gpu_percent: f32,        // GPU usage percent
    pub start_time: u64,         // Process start time (Unix timestamp)
}

// Backward compat alias
pub type WslProcess = Process;

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct MemoryInfo {
    pub total_mb: u64,
    pub used_mb: u64,
    pub available_mb: u64,
    pub percent: f32,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct DiskInfo {
    pub device: String,
    pub mount_point: String,
    pub total_gb: f32,
    pub used_gb: f32,
    pub available_gb: f32,
    pub percent: f32,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct NetworkInfo {
    pub interface: String,
    pub rx_bytes: u64,
    pub tx_bytes: u64,
    pub rx_packets: u64,
    pub tx_packets: u64,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct SystemStats {
    pub memory: MemoryInfo,
    pub cpu_percent: f32,
    pub processes: Vec<WslProcess>,
    pub load_avg: [f32; 3],
    pub uptime_secs: u64,
    pub kernel: String,
    pub hostname: String,
    pub cpu_count: u32,
    pub swap_total_mb: u64,
    pub swap_used_mb: u64,
    pub disks: Vec<DiskInfo>,
    pub networks: Vec<NetworkInfo>,
}

// ============================================================================
// WSL Command Execution
// ============================================================================

fn wsl_command() -> Command {
    let mut cmd = Command::new("wsl");
    #[cfg(windows)]
    cmd.creation_flags(CREATE_NO_WINDOW);
    cmd
}

fn run_wsl(args: &[&str]) -> Option<String> {
    wsl_command()
        .args(args)
        .output()
        .ok()
        .filter(|o| o.status.success())
        .map(|o| String::from_utf8_lossy(&o.stdout).into_owned())
}

// Fast Windows process enumeration using Win32 Toolhelp32 API
// This is ~100x faster than PowerShell Get-Process
#[cfg(windows)]
fn get_windows_processes() -> Vec<Process> {
    use std::mem::size_of;

    let mut processes = Vec::with_capacity(500);

    unsafe {
        // Create snapshot of all processes
        let snapshot = match CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0) {
            Ok(h) => h,
            Err(_) => return processes,
        };

        let mut entry = PROCESSENTRY32 {
            dwSize: size_of::<PROCESSENTRY32>() as u32,
            ..Default::default()
        };

        // Get first process
        if Process32First(snapshot, &mut entry).is_ok() {
            loop {
                let pid = entry.th32ProcessID;
                let ppid = entry.th32ParentProcessID;
                let threads = entry.cntThreads;

                // Skip system idle process
                if pid == 0 {
                    if Process32Next(snapshot, &mut entry).is_err() {
                        break;
                    }
                    continue;
                }

                // Get process name from szExeFile (null-terminated)
                let name_bytes: Vec<u8> = entry.szExeFile
                    .iter()
                    .take_while(|&&b| b != 0)
                    .map(|&b| b as u8)
                    .collect();
                let name = String::from_utf8_lossy(&name_bytes).to_string();

                // Get extended process info if we can open the process
                let mut memory_mb = 0.0f32;
                let mut disk_read_bytes = 0u64;
                let mut disk_write_bytes = 0u64;
                let mut priority = 0i32;
                let mut start_time = 0u64;

                if let Ok(proc_handle) = OpenProcess(
                    PROCESS_QUERY_INFORMATION | PROCESS_VM_READ,
                    false,
                    pid,
                ) {
                    // Memory info
                    let mut mem_counters = PROCESS_MEMORY_COUNTERS::default();
                    if K32GetProcessMemoryInfo(
                        proc_handle,
                        &mut mem_counters,
                        size_of::<PROCESS_MEMORY_COUNTERS>() as u32,
                    ).as_bool() {
                        memory_mb = mem_counters.WorkingSetSize as f32 / (1024.0 * 1024.0);
                    }

                    // Disk I/O counters
                    let mut io_counters = IO_COUNTERS::default();
                    if GetProcessIoCounters(proc_handle, &mut io_counters).is_ok() {
                        disk_read_bytes = io_counters.ReadTransferCount;
                        disk_write_bytes = io_counters.WriteTransferCount;
                    }

                    // Priority class
                    let prio_class = GetPriorityClass(proc_handle);
                    priority = match prio_class {
                        c if c == IDLE_PRIORITY_CLASS.0 => 4,
                        c if c == BELOW_NORMAL_PRIORITY_CLASS.0 => 6,
                        c if c == NORMAL_PRIORITY_CLASS.0 => 8,
                        c if c == ABOVE_NORMAL_PRIORITY_CLASS.0 => 10,
                        c if c == HIGH_PRIORITY_CLASS.0 => 13,
                        c if c == REALTIME_PRIORITY_CLASS.0 => 24,
                        _ => 8,
                    };

                    // Process start time
                    let mut creation_time = windows::Win32::Foundation::FILETIME::default();
                    let mut exit_time = windows::Win32::Foundation::FILETIME::default();
                    let mut kernel_time = windows::Win32::Foundation::FILETIME::default();
                    let mut user_time = windows::Win32::Foundation::FILETIME::default();
                    if GetProcessTimes(
                        proc_handle,
                        &mut creation_time,
                        &mut exit_time,
                        &mut kernel_time,
                        &mut user_time,
                    ).is_ok() {
                        // Convert FILETIME to Unix timestamp
                        let ft = ((creation_time.dwHighDateTime as u64) << 32) | creation_time.dwLowDateTime as u64;
                        // FILETIME is 100-nanosecond intervals since Jan 1, 1601
                        // Unix time is seconds since Jan 1, 1970
                        // Difference is 11644473600 seconds
                        start_time = (ft / 10_000_000).saturating_sub(11644473600);
                    }

                    let _ = CloseHandle(proc_handle);
                }

                processes.push(Process {
                    pid,
                    ppid,
                    user: String::new(),
                    cpu_percent: 0.0, // CPU requires sampling over time
                    memory_mb,
                    memory_percent: 0.0,
                    command: name.clone(),
                    short_name: name.trim_end_matches(".exe").to_string(),
                    state: "R".to_string(), // Assume running
                    exe_path: String::new(),
                    is_system: false,
                    source: "windows".to_string(),
                    disk_read_bytes,
                    disk_write_bytes,
                    net_rx_bytes: 0,
                    net_tx_bytes: 0,
                    threads,
                    handles: 0, // Would need NtQuerySystemInformation
                    priority,
                    gpu_percent: 0.0,
                    start_time,
                });

                // Get next process
                if Process32Next(snapshot, &mut entry).is_err() {
                    break;
                }
            }
        }

        let _ = CloseHandle(snapshot);
    }

    processes
}

#[cfg(not(windows))]
fn get_windows_processes() -> Vec<Process> {
    Vec::new()
}

// ============================================================================
// Parsing Functions
// ============================================================================

fn parse_meminfo(s: &str) -> MemoryInfo {
    let extract = |prefix: &str| -> u64 {
        s.lines()
            .find(|l| l.starts_with(prefix))
            .and_then(|l| l.split_whitespace().nth(1))
            .and_then(|v| v.parse::<u64>().ok())
            .map(|kb| kb / 1024)
            .unwrap_or(0)
    };

    let total = extract("MemTotal:");
    let available = extract("MemAvailable:");
    let used = total.saturating_sub(available);
    let percent = if total > 0 {
        used as f32 / total as f32 * 100.0
    } else {
        0.0
    };

    MemoryInfo {
        total_mb: total,
        used_mb: used,
        available_mb: available,
        percent,
    }
}

fn parse_loadavg(s: &str) -> [f32; 3] {
    s.split_whitespace()
        .take(3)
        .filter_map(|v| v.parse().ok())
        .collect::<Vec<_>>()
        .try_into()
        .unwrap_or([0.0; 3])
}

fn parse_ps_line(line: &str) -> Option<WslProcess> {
    // Format: USER PID PPID %CPU %MEM STAT NI NLWP RSS COMMAND
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() >= 10 {
        let command = parts[9..].join(" ");
        let short_name = command
            .split_whitespace()
            .next()
            .and_then(|s| s.split('/').last())
            .unwrap_or("")
            .to_string();

        let user = parts[0].to_string();
        let state = parts[5].to_string();

        // System processes: root, daemon, nobody, or system-like users
        let is_system = matches!(user.as_str(), "root" | "daemon" | "nobody" | "systemd+" | "syslog" | "messagebus" | "_apt" | "www-data");

        // Parse nice value (NI column) and thread count (NLWP column)
        let nice: i32 = parts[6].parse().unwrap_or(0);
        let threads: u32 = parts[7].parse().unwrap_or(1);

        Some(WslProcess {
            user,
            pid: parts[1].parse().unwrap_or(0),
            ppid: parts[2].parse().unwrap_or(0),
            cpu_percent: parts[3].parse().unwrap_or(0.0),
            memory_percent: parts[4].parse().unwrap_or(0.0),
            memory_mb: parts[8].parse::<f32>().unwrap_or(0.0) / 1024.0, // RSS in KB
            command,
            short_name,
            state,
            exe_path: String::new(),
            is_system,
            source: String::new(),
            disk_read_bytes: 0,
            disk_write_bytes: 0,
            net_rx_bytes: 0,
            net_tx_bytes: 0,
            threads,
            handles: 0,
            priority: nice,
            gpu_percent: 0.0,
            start_time: 0,
        })
    } else {
        None
    }
}

fn parse_ps_output(s: &str) -> Vec<WslProcess> {
    s.lines().skip(1).filter_map(parse_ps_line).collect()
}

// ============================================================================
// Tauri Commands
// ============================================================================

fn parse_df_line(line: &str) -> Option<DiskInfo> {
    let parts: Vec<&str> = line.split_whitespace().collect();
    if parts.len() >= 6 && parts[0].starts_with('/') {
        let total_kb: f32 = parts[1].parse().unwrap_or(0.0);
        let used_kb: f32 = parts[2].parse().unwrap_or(0.0);
        let avail_kb: f32 = parts[3].parse().unwrap_or(0.0);
        let percent_str = parts[4].trim_end_matches('%');

        Some(DiskInfo {
            device: parts[0].to_string(),
            mount_point: parts[5].to_string(),
            total_gb: total_kb / 1024.0 / 1024.0,
            used_gb: used_kb / 1024.0 / 1024.0,
            available_gb: avail_kb / 1024.0 / 1024.0,
            percent: percent_str.parse().unwrap_or(0.0),
        })
    } else {
        None
    }
}

fn parse_df_output(s: &str) -> Vec<DiskInfo> {
    s.lines()
        .skip(1)
        .filter_map(parse_df_line)
        .filter(|d| !d.mount_point.starts_with("/snap") && !d.mount_point.starts_with("/boot"))
        .collect()
}

fn parse_net_dev(s: &str) -> Vec<NetworkInfo> {
    s.lines()
        .skip(2) // Skip header lines
        .filter_map(|line| {
            let line = line.trim();
            let (iface, rest) = line.split_once(':')?;
            let parts: Vec<&str> = rest.split_whitespace().collect();
            if parts.len() >= 10 {
                Some(NetworkInfo {
                    interface: iface.trim().to_string(),
                    rx_bytes: parts[0].parse().unwrap_or(0),
                    rx_packets: parts[1].parse().unwrap_or(0),
                    tx_bytes: parts[8].parse().unwrap_or(0),
                    tx_packets: parts[9].parse().unwrap_or(0),
                })
            } else {
                None
            }
        })
        .filter(|n| n.interface != "lo") // Exclude loopback
        .collect()
}

#[tauri::command]
async fn get_stats(shell_state: State<'_, ShellState>) -> Result<SystemStats, String> {
    let shell_state = shell_state.0.clone();

    // Run on background thread to avoid blocking
    tokio::task::spawn_blocking(move || {
        let mut shell_guard = shell_state.lock();

        // Initialize shell if needed
        if shell_guard.is_none() {
            match WslShell::new(None) {
                Ok(shell) => *shell_guard = Some(shell),
                Err(e) => return Err(format!("Shell init failed: {}", e)),
            }
        }

        let shell = shell_guard.as_mut().ok_or("Shell not available")?;
        get_stats_with_shell(shell)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

fn get_stats_with_shell(shell: &mut WslShell) -> Result<SystemStats, String> {
    // Get all info in one bash call for performance
    let info = shell.run(r#"
        cat /proc/meminfo
        echo "---SEPARATOR---"
        cat /proc/loadavg
        echo "---SEPARATOR---"
        cat /proc/uptime
        echo "---SEPARATOR---"
        uname -r
        echo "---SEPARATOR---"
        hostname
        echo "---SEPARATOR---"
        nproc
        echo "---SEPARATOR---"
        ps -eo user,pid,ppid,%cpu,%mem,stat,ni,nlwp,rss,args --sort=-%mem | head -100
        echo "---SEPARATOR---"
        df -k 2>/dev/null | grep -E '^/'
        echo "---SEPARATOR---"
        cat /proc/net/dev
    "#)?;

    let parts: Vec<&str> = info.split("---SEPARATOR---").collect();
    if parts.len() < 9 {
        return Err("Invalid response from WSL".to_string());
    }

    let memory = parse_meminfo(parts[0]);
    let load_avg = parse_loadavg(parts[1].trim());

    let uptime_secs = parts[2]
        .trim()
        .split_whitespace()
        .next()
        .and_then(|s| s.parse::<f64>().ok())
        .unwrap_or(0.0) as u64;

    let kernel = parts[3].trim().to_string();
    let hostname = parts[4].trim().to_string();
    let cpu_count = parts[5].trim().parse().unwrap_or(1);
    let processes = parse_ps_output(parts[6]);
    let disks = parse_df_output(parts[7]);
    let networks = parse_net_dev(parts[8]);

    let cpu_percent = processes
        .iter()
        .map(|p| p.cpu_percent)
        .sum::<f32>()
        .min(100.0);

    // Parse swap from meminfo
    let swap_total_mb = parse_meminfo_field(parts[0], "SwapTotal:");
    let swap_free_mb = parse_meminfo_field(parts[0], "SwapFree:");
    let swap_used_mb = swap_total_mb.saturating_sub(swap_free_mb);

    Ok(SystemStats {
        memory,
        cpu_percent,
        processes,
        load_avg,
        uptime_secs,
        kernel,
        hostname,
        cpu_count,
        swap_total_mb,
        swap_used_mb,
        disks,
        networks,
    })
}

fn parse_meminfo_field(s: &str, field: &str) -> u64 {
    s.lines()
        .find(|l| l.starts_with(field))
        .and_then(|l| l.split_whitespace().nth(1))
        .and_then(|v| v.parse::<u64>().ok())
        .map(|kb| kb / 1024)
        .unwrap_or(0)
}

// Brief overview stats for a single source
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SourceOverview {
    pub source_id: String,
    pub name: String,
    pub icon: String,
    pub cpu_percent: f32,
    pub memory_percent: f32,
    pub memory_used_mb: u64,
    pub memory_total_mb: u64,
    pub process_count: usize,
    pub is_available: bool,
}

// Get stats for a specific source (Windows, WSL:distro, etc.)
#[tauri::command]
async fn get_stats_for_source(source_id: String) -> Result<SystemStats, String> {
    if source_id == "windows" {
        // Get Windows stats
        tokio::task::spawn_blocking(move || get_windows_stats())
            .await
            .map_err(|e| format!("Task failed: {}", e))?
    } else if source_id.starts_with("wsl:") {
        let distro = source_id.strip_prefix("wsl:").unwrap_or("").to_string();
        tokio::task::spawn_blocking(move || get_wsl_stats_for_distro(&distro))
            .await
            .map_err(|e| format!("Task failed: {}", e))?
    } else if source_id == "all" {
        // Combined view - get all and merge
        tokio::task::spawn_blocking(get_combined_stats)
            .await
            .map_err(|e| format!("Task failed: {}", e))?
    } else {
        Err(format!("Unknown source: {}", source_id))
    }
}

// Get brief overview of all systems
#[tauri::command]
async fn get_overview() -> Result<Vec<SourceOverview>, String> {
    tokio::task::spawn_blocking(|| {
        let mut overviews = Vec::new();

        // Windows overview
        if let Ok(stats) = get_windows_stats() {
            overviews.push(SourceOverview {
                source_id: "windows".to_string(),
                name: "Windows".to_string(),
                icon: "🪟".to_string(),
                cpu_percent: stats.cpu_percent,
                memory_percent: stats.memory.percent,
                memory_used_mb: stats.memory.used_mb,
                memory_total_mb: stats.memory.total_mb,
                process_count: stats.processes.len(),
                is_available: true,
            });
        }

        // WSL distros overview
        if let Ok(distros) = list_distros_internal() {
            for distro in distros {
                if distro.is_running {
                    if let Ok(stats) = get_wsl_stats_for_distro(&distro.name) {
                        overviews.push(SourceOverview {
                            source_id: format!("wsl:{}", distro.name),
                            name: distro.name,
                            icon: "🐧".to_string(),
                            cpu_percent: stats.cpu_percent,
                            memory_percent: stats.memory.percent,
                            memory_used_mb: stats.memory.used_mb,
                            memory_total_mb: stats.memory.total_mb,
                            process_count: stats.processes.len(),
                            is_available: true,
                        });
                    }
                }
            }
        }

        Ok(overviews)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// Get Windows system stats
fn get_windows_stats() -> Result<SystemStats, String> {
    let processes = get_windows_processes();

    // Use native Win32 API for memory - instant, no PowerShell overhead
    #[cfg(windows)]
    let memory = get_native_memory_info().unwrap_or(MemoryInfo {
        total_mb: 0, used_mb: 0, available_mb: 0, percent: 0.0,
    });

    #[cfg(not(windows))]
    let memory = MemoryInfo {
        total_mb: 0, used_mb: 0, available_mb: 0, percent: 0.0,
    };

    // Calculate CPU from process data (sum of all process CPU %)
    let cpu_percent: f32 = processes.iter()
        .map(|p| p.cpu_percent)
        .sum::<f32>()
        .min(100.0 * num_cpus::get() as f32)
        / num_cpus::get() as f32;

    Ok(SystemStats {
        memory,
        cpu_percent,
        processes,
        load_avg: [0.0; 3],
        uptime_secs: 0,
        kernel: "Windows".to_string(),
        hostname: std::env::var("COMPUTERNAME").unwrap_or_default(),
        cpu_count: num_cpus::get() as u32,
        swap_total_mb: 0,
        swap_used_mb: 0,
        disks: Vec::new(),
        networks: Vec::new(),
    })
}

// Get WSL stats for a specific distro (without persistent shell)
fn get_wsl_stats_for_distro(distro: &str) -> Result<SystemStats, String> {
    let mut cmd = Command::new("wsl");
    cmd.args(["-d", distro, "-e", "bash", "-c", r#"
        cat /proc/meminfo
        echo "---SEPARATOR---"
        cat /proc/loadavg
        echo "---SEPARATOR---"
        cat /proc/uptime
        echo "---SEPARATOR---"
        uname -r
        echo "---SEPARATOR---"
        hostname
        echo "---SEPARATOR---"
        nproc
        echo "---SEPARATOR---"
        ps -eo user,pid,ppid,%cpu,%mem,stat,ni,nlwp,rss,args --sort=-%mem | head -100
    "#]);

    #[cfg(windows)]
    cmd.creation_flags(CREATE_NO_WINDOW);

    let output = cmd
        .output()
        .map_err(|e| format!("WSL command failed: {}", e))?;

    if !output.status.success() {
        return Err(format!("WSL distro '{}' not available", distro));
    }

    let info = String::from_utf8_lossy(&output.stdout);
    let parts: Vec<&str> = info.split("---SEPARATOR---").collect();

    if parts.len() < 7 {
        return Err("Invalid response from WSL".to_string());
    }

    let memory = parse_meminfo(parts[0]);
    let load_avg = parse_loadavg(parts[1].trim());
    let uptime_secs = parts[2]
        .trim()
        .split_whitespace()
        .next()
        .and_then(|s| s.parse::<f64>().ok())
        .unwrap_or(0.0) as u64;
    let kernel = parts[3].trim().to_string();
    let hostname = parts[4].trim().to_string();
    let cpu_count = parts[5].trim().parse().unwrap_or(1);

    let mut processes = parse_ps_output(parts[6]);
    // Tag processes with source
    for p in &mut processes {
        p.source = format!("wsl:{}", distro);
    }

    let cpu_percent = processes
        .iter()
        .map(|p| p.cpu_percent)
        .sum::<f32>()
        .min(100.0);

    let swap_total_mb = parse_meminfo_field(parts[0], "SwapTotal:");
    let swap_free_mb = parse_meminfo_field(parts[0], "SwapFree:");

    Ok(SystemStats {
        memory,
        cpu_percent,
        processes,
        load_avg,
        uptime_secs,
        kernel,
        hostname,
        cpu_count,
        swap_total_mb,
        swap_used_mb: swap_total_mb.saturating_sub(swap_free_mb),
        disks: Vec::new(),
        networks: Vec::new(),
    })
}

// Build hierarchical system tree - returns cached data for instant response
#[tauri::command]
async fn get_system_tree(cache_state: State<'_, CacheState>) -> Result<SystemTreeNode, String> {
    // Check cache first (instant response)
    let cached = {
        let cache = cache_state.0.lock();
        cache.system_tree.clone()
    };

    if let Some(tree) = cached {
        return Ok(tree);
    }

    // First call - build synchronously but this should be rare
    let cache_arc = cache_state.0.clone();
    let result = tokio::task::spawn_blocking(move || {
        let tree = build_system_tree()?;

        // Cache the result
        let mut cache = cache_arc.lock();
        cache.system_tree = Some(tree.clone());
        cache.last_update = Some(Instant::now());

        Ok::<_, String>(tree)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))??;

    Ok(result)
}

// Trigger background refresh of system tree (non-blocking)
#[tauri::command]
async fn refresh_system_tree(cache_state: State<'_, CacheState>) -> Result<(), String> {
    let cache_arc = cache_state.0.clone();

    // Don't spawn if already updating
    {
        let cache = cache_arc.lock();
        if cache.is_updating {
            return Ok(());
        }
    }

    // Mark as updating
    {
        let mut cache = cache_arc.lock();
        cache.is_updating = true;
    }

    // Spawn background task to update cache
    let cache_for_task = cache_arc.clone();
    tokio::task::spawn_blocking(move || {
        if let Ok(tree) = build_system_tree() {
            let mut cache = cache_for_task.lock();
            cache.system_tree = Some(tree);
            cache.last_update = Some(Instant::now());
            cache.is_updating = false;
        } else {
            let mut cache = cache_for_task.lock();
            cache.is_updating = false;
        }
    });

    Ok(())
}

fn build_system_tree() -> Result<SystemTreeNode, String> {
    use std::collections::HashMap;

    // Root: Windows host
    let mut root = SystemTreeNode::new_host("windows", "Windows", "🪟");

    // Get Windows processes
    let win_procs = get_windows_processes();

    // Find vmmem processes (WSL container)
    let vmmem_pids: Vec<u32> = win_procs
        .iter()
        .filter(|p| p.short_name.to_lowercase().contains("vmmem"))
        .map(|p| p.pid)
        .collect();

    // Group Windows processes by parent for tree view
    let mut win_by_parent: HashMap<u32, Vec<&Process>> = HashMap::new();
    let mut win_roots: Vec<&Process> = Vec::new();

    for p in &win_procs {
        // Skip vmmem processes (handled separately)
        if p.short_name.to_lowercase().contains("vmmem") {
            continue;
        }

        // Find if parent exists in our list
        let parent_exists = win_procs.iter().any(|other| other.pid == p.ppid && other.pid != p.pid);

        if parent_exists && p.ppid != 0 {
            win_by_parent.entry(p.ppid).or_default().push(p);
        } else {
            win_roots.push(p);
        }
    }

    // Build Windows process tree (group by short_name for apps like Edge)
    let mut app_groups: HashMap<String, Vec<&Process>> = HashMap::new();
    for p in &win_roots {
        app_groups.entry(p.short_name.clone()).or_default().push(p);
    }

    // Add Windows apps as children
    for (app_name, procs) in app_groups {
        if procs.len() == 1 && win_by_parent.get(&procs[0].pid).map(|c| c.len()).unwrap_or(0) == 0 {
            // Single process with no children - add directly
            let node = SystemTreeNode::from_process(procs[0], "windows", 1);
            root.children.push(node);
        } else {
            // Multiple instances or has children - create group
            let mut group = SystemTreeNode {
                id: format!("windows:app:{}", app_name),
                name: format!("{} ({})", app_name, procs.len()),
                node_type: "app".to_string(),
                icon: "📦".to_string(),
                parent_id: Some("windows".to_string()),
                host_pid: None,
                depth: 1,
                is_expanded: false,
                child_count: procs.len(),
                cpu_percent: 0.0,
                memory_mb: 0.0,
                process_count: 0,
                pid: None,
                ppid: None,
                user: None,
                state: None,
                command: None,
                children: Vec::new(),
            };

            // Add processes and their children
            for p in procs {
                let mut proc_node = SystemTreeNode::from_process(p, &group.id, 2);

                // Add children of this process
                if let Some(children) = win_by_parent.get(&p.pid) {
                    for child in children {
                        let child_node = SystemTreeNode::from_process(child, &proc_node.id, 3);
                        proc_node.children.push(child_node);
                    }
                }
                proc_node.child_count = proc_node.children.len();

                group.children.push(proc_node);
            }

            group.aggregate_stats();
            root.children.push(group);
        }
    }

    // Add WSL subsystems under vmmem
    if !vmmem_pids.is_empty() {
        let vmmem_pid = vmmem_pids[0];
        let vmmem_mem: f32 = win_procs
            .iter()
            .filter(|p| vmmem_pids.contains(&p.pid))
            .map(|p| p.memory_mb)
            .sum();

        let mut vmmem_node = SystemTreeNode::new_subsystem(
            "vmmem",
            "Virtual Machines",
            "vm-container",
            "🖥️",
            "windows",
            Some(vmmem_pid),
        );
        vmmem_node.memory_mb = vmmem_mem;

        // Add each WSL distro
        if let Ok(distros) = list_distros_internal() {
            for distro in distros.iter().filter(|d| d.is_running) {
                if let Ok(wsl_stats) = get_wsl_stats_for_distro(&distro.name) {
                    let mut wsl_node = SystemTreeNode::new_subsystem(
                        &format!("wsl:{}", distro.name),
                        &distro.name,
                        "wsl",
                        "🐧",
                        "vmmem",
                        None,
                    );
                    wsl_node.depth = 2;

                    // Group processes by user
                    let mut by_user: HashMap<String, Vec<&Process>> = HashMap::new();
                    for p in &wsl_stats.processes {
                        by_user.entry(p.user.clone()).or_default().push(p);
                    }

                    // Add user groups
                    for (user, user_procs) in by_user {
                        let mut user_node = SystemTreeNode::new_user(&user, &wsl_node.id, 3);

                        // Build process tree for this user
                        let user_pids: std::collections::HashSet<u32> = user_procs.iter().map(|p| p.pid).collect();
                        let mut proc_by_parent: HashMap<u32, Vec<&Process>> = HashMap::new();
                        let mut user_roots: Vec<&Process> = Vec::new();

                        for p in &user_procs {
                            if user_pids.contains(&p.ppid) && p.ppid != p.pid {
                                proc_by_parent.entry(p.ppid).or_default().push(p);
                            } else {
                                user_roots.push(p);
                            }
                        }

                        // Add root processes with children
                        fn add_proc_tree(
                            p: &Process,
                            parent_id: &str,
                            depth: u32,
                            by_parent: &HashMap<u32, Vec<&Process>>,
                        ) -> SystemTreeNode {
                            let mut node = SystemTreeNode::from_process(p, parent_id, depth);
                            if let Some(children) = by_parent.get(&p.pid) {
                                for child in children {
                                    node.children.push(add_proc_tree(child, &node.id, depth + 1, by_parent));
                                }
                            }
                            node.child_count = node.children.len();
                            node
                        }

                        for p in user_roots {
                            user_node.children.push(add_proc_tree(p, &user_node.id, 4, &proc_by_parent));
                        }

                        user_node.aggregate_stats();
                        wsl_node.children.push(user_node);
                    }

                    wsl_node.aggregate_stats();
                    vmmem_node.children.push(wsl_node);
                }
            }
        }

        vmmem_node.aggregate_stats();
        root.children.push(vmmem_node);
    }

    root.aggregate_stats();
    Ok(root)
}

// Get combined stats from all sources
fn get_combined_stats() -> Result<SystemStats, String> {
    let mut all_processes = Vec::new();
    let mut total_cpu = 0.0;
    let mut count = 0;

    // Get Windows processes
    if let Ok(win_stats) = get_windows_stats() {
        all_processes.extend(win_stats.processes);
        total_cpu += win_stats.cpu_percent;
        count += 1;
    }

    // Get WSL processes from running distros
    if let Ok(distros) = list_distros_internal() {
        for distro in distros.iter().filter(|d| d.is_running) {
            if let Ok(wsl_stats) = get_wsl_stats_for_distro(&distro.name) {
                all_processes.extend(wsl_stats.processes);
                total_cpu += wsl_stats.cpu_percent;
                count += 1;
            }
        }
    }

    // Use Windows memory as primary (host)
    let memory = get_windows_stats()
        .map(|s| s.memory)
        .unwrap_or_default();

    Ok(SystemStats {
        memory,
        cpu_percent: if count > 0 { total_cpu / count as f32 } else { 0.0 },
        processes: all_processes,
        load_avg: [0.0; 3],
        uptime_secs: 0,
        kernel: "Combined".to_string(),
        hostname: "All Systems".to_string(),
        cpu_count: num_cpus::get() as u32,
        swap_total_mb: 0,
        swap_used_mb: 0,
        disks: Vec::new(),
        networks: Vec::new(),
    })
}

#[tauri::command]
fn kill_process(pid: u32) -> Result<(), String> {
    run_wsl(&["-e", "kill", "-9", &pid.to_string()])
        .ok_or_else(|| format!("Failed to kill process {}", pid))?;
    Ok(())
}

#[tauri::command]
fn kill_tree(pid: u32) -> Result<(), String> {
    let script = format!("pkill -9 -P {}; kill -9 {}", pid, pid);
    run_wsl(&["-e", "bash", "-c", &script])
        .ok_or_else(|| format!("Failed to kill process tree {}", pid))?;
    Ok(())
}

#[tauri::command]
fn kill_pattern(pattern: String) -> Result<(), String> {
    run_wsl(&["-e", "pkill", "-9", "-f", &pattern])
        .ok_or_else(|| format!("Failed to kill pattern {}", pattern))?;
    Ok(())
}

// List all available system sources (Windows, WSL distros, etc.)
#[tauri::command]
fn list_sources() -> Result<Vec<AvailableSource>, String> {
    let mut sources = Vec::new();

    // Always add Windows
    sources.push(AvailableSource {
        id: "windows".to_string(),
        name: "Windows".to_string(),
        source_type: "windows".to_string(),
        is_available: true,
        icon: "🪟".to_string(),
    });

    // Add "All" combined view
    sources.push(AvailableSource {
        id: "all".to_string(),
        name: "All Systems".to_string(),
        source_type: "all".to_string(),
        is_available: true,
        icon: "🌐".to_string(),
    });

    // Get WSL distros
    if let Ok(distros) = list_distros_internal() {
        for distro in distros {
            sources.push(AvailableSource {
                id: format!("wsl:{}", distro.name),
                name: distro.name.clone(),
                source_type: "wsl".to_string(),
                is_available: distro.is_running,
                icon: "🐧".to_string(),
            });
        }
    }

    Ok(sources)
}

fn list_distros_internal() -> Result<Vec<WslDistro>, String> {
    // Get list of all distros with verbose info
    let mut cmd = Command::new("wsl");
    cmd.args(["--list", "--verbose"]);

    #[cfg(windows)]
    cmd.creation_flags(CREATE_NO_WINDOW);

    let output = cmd
        .output()
        .map_err(|e| format!("Failed to list distros: {}", e))?;

    if !output.status.success() {
        return Err("WSL command failed".to_string());
    }

    // WSL outputs UTF-16 on Windows
    let stdout = String::from_utf8_lossy(&output.stdout);

    let distros: Vec<WslDistro> = stdout
        .lines()
        .skip(1)
        .filter_map(|line| {
            let line = line.replace('\0', "").trim().to_string();
            if line.is_empty() {
                return None;
            }

            let is_default = line.starts_with('*');
            let line = line.trim_start_matches('*').trim();

            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.is_empty() {
                return None;
            }

            let name = parts[0].to_string();
            let is_running = parts.get(1).map(|s| *s == "Running").unwrap_or(false);

            Some(WslDistro {
                name,
                is_default,
                is_running,
            })
        })
        .collect();

    Ok(distros)
}

#[tauri::command]
fn list_distros() -> Result<Vec<WslDistro>, String> {
    list_distros_internal()
}

#[tauri::command]
fn get_current_distro(distro_state: State<DistroState>) -> String {
    distro_state.0.lock().clone()
}

#[tauri::command]
async fn set_distro(
    distro: String,
    shell_state: State<'_, ShellState>,
    distro_state: State<'_, DistroState>,
) -> Result<(), String> {
    let shell_state = shell_state.0.clone();
    let distro_state_clone = distro_state.0.clone();
    let distro_clone = distro.clone();

    // Recreate shell with new distro on background thread
    tokio::task::spawn_blocking(move || {
        let mut shell_guard = shell_state.lock();

        // Create new shell with selected distro
        let distro_opt = if distro_clone == "default" {
            None
        } else {
            Some(distro_clone.as_str())
        };

        match WslShell::new(distro_opt) {
            Ok(shell) => {
                *shell_guard = Some(shell);
                *distro_state_clone.lock() = distro_clone;
                Ok(())
            }
            Err(e) => Err(format!("Failed to switch distro: {}", e)),
        }
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// Windows Service info
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WindowsService {
    name: String,
    display_name: String,
    status: String,
    start_type: String,
}

#[tauri::command]
async fn get_windows_services() -> Result<Vec<WindowsService>, String> {
    #[cfg(windows)]
    {
        use tokio::task::spawn_blocking;

        spawn_blocking(|| {
            let output = Command::new("powershell")
                .args([
                    "-NoProfile",
                    "-Command",
                    "Get-Service | Select-Object Name, DisplayName, Status, StartType | ConvertTo-Json -Compress",
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| format!("PowerShell failed: {}", e))?;

            if !output.status.success() {
                return Err("PowerShell command failed".to_string());
            }

            let json = String::from_utf8_lossy(&output.stdout);

            // Parse the JSON output
            #[derive(Deserialize)]
            struct PSService {
                #[serde(alias = "Name")]
                name: String,
                #[serde(alias = "DisplayName")]
                display_name: Option<String>,
                #[serde(alias = "Status")]
                status: Option<i32>,
                #[serde(alias = "StartType")]
                start_type: Option<i32>,
            }

            let ps_services: Vec<PSService> = serde_json::from_str(&json)
                .map_err(|e| format!("JSON parse error: {}", e))?;

            let services: Vec<WindowsService> = ps_services
                .into_iter()
                .map(|s| {
                    let status = match s.status {
                        Some(4) => "Running".to_string(),
                        Some(1) => "Stopped".to_string(),
                        Some(2) => "StartPending".to_string(),
                        Some(3) => "StopPending".to_string(),
                        _ => "Unknown".to_string(),
                    };
                    let start_type = match s.start_type {
                        Some(2) => "Automatic".to_string(),
                        Some(3) => "Manual".to_string(),
                        Some(4) => "Disabled".to_string(),
                        Some(0) => "Boot".to_string(),
                        Some(1) => "System".to_string(),
                        _ => "Unknown".to_string(),
                    };
                    WindowsService {
                        name: s.name,
                        display_name: s.display_name.unwrap_or_default(),
                        status,
                        start_type,
                    }
                })
                .collect();

            Ok(services)
        })
        .await
        .map_err(|e| format!("Task failed: {}", e))?
    }

    #[cfg(not(windows))]
    {
        Ok(vec![])
    }
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct LinuxService {
    unit: String,
    load: String,
    active: String,
    sub: String,
    description: String,
}

#[tauri::command]
async fn get_linux_services() -> Result<Vec<LinuxService>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(|| {
        // Use WSL to get systemd services
        let output = Command::new("wsl")
            .args([
                "-e",
                "systemctl",
                "list-units",
                "--type=service",
                "--all",
                "--no-pager",
                "--no-legend",
            ])
            .creation_flags(CREATE_NO_WINDOW)
            .output()
            .map_err(|e| format!("WSL systemctl failed: {}", e))?;

        if !output.status.success() {
            return Err("systemctl command failed".to_string());
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        let services: Vec<LinuxService> = stdout
            .lines()
            .filter(|line| !line.is_empty())
            .filter_map(|line| {
                // Format: UNIT LOAD ACTIVE SUB DESCRIPTION...
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 5 {
                    Some(LinuxService {
                        unit: parts[0].to_string(),
                        load: parts[1].to_string(),
                        active: parts[2].to_string(),
                        sub: parts[3].to_string(),
                        description: parts[4..].join(" "),
                    })
                } else {
                    None
                }
            })
            .collect();

        Ok(services)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Network Connections
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct NetworkConnection {
    pub protocol: String,      // TCP, UDP
    pub local_addr: String,
    pub local_port: u16,
    pub remote_addr: String,
    pub remote_port: u16,
    pub state: String,         // ESTABLISHED, LISTEN, TIME_WAIT, etc.
    pub pid: u32,
    pub process_name: String,
}

#[tauri::command]
async fn get_network_connections() -> Result<Vec<NetworkConnection>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(|| {
        let mut connections = Vec::new();

        // Use netstat via PowerShell for Windows
        #[cfg(windows)]
        {
            let output = Command::new("powershell")
                .args([
                    "-NoProfile",
                    "-Command",
                    "Get-NetTCPConnection | Select-Object LocalAddress,LocalPort,RemoteAddress,RemotePort,State,OwningProcess | ConvertTo-Json -Compress",
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| format!("PowerShell failed: {}", e))?;

            if output.status.success() {
                let json = String::from_utf8_lossy(&output.stdout);
                #[derive(Deserialize)]
                struct TcpConn {
                    #[serde(alias = "LocalAddress")]
                    local_address: Option<String>,
                    #[serde(alias = "LocalPort")]
                    local_port: Option<u16>,
                    #[serde(alias = "RemoteAddress")]
                    remote_address: Option<String>,
                    #[serde(alias = "RemotePort")]
                    remote_port: Option<u16>,
                    #[serde(alias = "State")]
                    state: Option<i32>,
                    #[serde(alias = "OwningProcess")]
                    owning_process: Option<u32>,
                }

                if let Ok(conns) = serde_json::from_str::<Vec<TcpConn>>(&json) {
                    for c in conns {
                        let state = match c.state {
                            Some(1) => "Closed",
                            Some(2) => "Listen",
                            Some(3) => "SynSent",
                            Some(4) => "SynReceived",
                            Some(5) => "Established",
                            Some(6) => "FinWait1",
                            Some(7) => "FinWait2",
                            Some(8) => "CloseWait",
                            Some(9) => "Closing",
                            Some(10) => "LastAck",
                            Some(11) => "TimeWait",
                            Some(12) => "DeleteTCB",
                            _ => "Unknown",
                        };

                        connections.push(NetworkConnection {
                            protocol: "TCP".to_string(),
                            local_addr: c.local_address.unwrap_or_default(),
                            local_port: c.local_port.unwrap_or(0),
                            remote_addr: c.remote_address.unwrap_or_default(),
                            remote_port: c.remote_port.unwrap_or(0),
                            state: state.to_string(),
                            pid: c.owning_process.unwrap_or(0),
                            process_name: String::new(),
                        });
                    }
                }
            }
        }

        Ok(connections)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Startup Apps
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StartupApp {
    pub name: String,
    pub command: String,
    pub location: String,  // Registry key or startup folder
    pub enabled: bool,
}

#[tauri::command]
async fn get_startup_apps() -> Result<Vec<StartupApp>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(|| {
        let mut apps = Vec::new();

        #[cfg(windows)]
        {
            let output = Command::new("powershell")
                .args([
                    "-NoProfile",
                    "-Command",
                    r#"Get-CimInstance Win32_StartupCommand | Select-Object Name,Command,Location | ConvertTo-Json -Compress"#,
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| format!("PowerShell failed: {}", e))?;

            if output.status.success() {
                let json = String::from_utf8_lossy(&output.stdout);
                #[derive(Deserialize)]
                struct StartupCmd {
                    #[serde(alias = "Name")]
                    name: Option<String>,
                    #[serde(alias = "Command")]
                    command: Option<String>,
                    #[serde(alias = "Location")]
                    location: Option<String>,
                }

                if let Ok(cmds) = serde_json::from_str::<Vec<StartupCmd>>(&json) {
                    for c in cmds {
                        apps.push(StartupApp {
                            name: c.name.unwrap_or_default(),
                            command: c.command.unwrap_or_default(),
                            location: c.location.unwrap_or_default(),
                            enabled: true,
                        });
                    }
                }
            }
        }

        Ok(apps)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Per-Core CPU
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CpuCore {
    pub core_id: u32,
    pub percent: f32,
}

#[tauri::command]
async fn get_cpu_cores() -> Result<Vec<CpuCore>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(|| {
        let mut cores = Vec::new();

        #[cfg(windows)]
        {
            // Use WMI for per-core CPU
            let output = Command::new("powershell")
                .args([
                    "-NoProfile",
                    "-Command",
                    r#"Get-WmiObject Win32_PerfFormattedData_PerfOS_Processor | Where-Object { $_.Name -ne '_Total' } | Select-Object Name,PercentProcessorTime | ConvertTo-Json -Compress"#,
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| format!("PowerShell failed: {}", e))?;

            if output.status.success() {
                let json = String::from_utf8_lossy(&output.stdout);
                #[derive(Deserialize)]
                struct CoreInfo {
                    #[serde(alias = "Name")]
                    #[allow(dead_code)]
                    name: Option<String>,
                    #[serde(alias = "PercentProcessorTime")]
                    percent: Option<u64>,
                }

                if let Ok(infos) = serde_json::from_str::<Vec<CoreInfo>>(&json) {
                    for (i, info) in infos.iter().enumerate() {
                        cores.push(CpuCore {
                            core_id: i as u32,
                            percent: info.percent.unwrap_or(0) as f32,
                        });
                    }
                }
            }
        }

        Ok(cores)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Process Priority Control
// ============================================================================

#[tauri::command]
async fn set_process_priority(pid: u32, priority: i32, source: String) -> Result<(), String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        if source == "windows" {
            #[cfg(windows)]
            unsafe {
                use windows::Win32::System::Threading::PROCESS_SET_INFORMATION;

                let proc_handle = OpenProcess(PROCESS_SET_INFORMATION, false, pid)
                    .map_err(|e| format!("Cannot open process: {}", e))?;

                let priority_class = match priority {
                    p if p <= 4 => IDLE_PRIORITY_CLASS,
                    p if p <= 6 => BELOW_NORMAL_PRIORITY_CLASS,
                    p if p <= 8 => NORMAL_PRIORITY_CLASS,
                    p if p <= 10 => ABOVE_NORMAL_PRIORITY_CLASS,
                    p if p <= 13 => HIGH_PRIORITY_CLASS,
                    _ => REALTIME_PRIORITY_CLASS,
                };

                SetPriorityClass(proc_handle, priority_class)
                    .map_err(|e| format!("Failed to set priority: {}", e))?;

                let _ = CloseHandle(proc_handle);
            }
        } else {
            // Linux: use renice via WSL
            let nice_value = priority.clamp(-20, 19);
            let _ = Command::new("wsl")
                .args(["-e", "sudo", "renice", &nice_value.to_string(), "-p", &pid.to_string()])
                .creation_flags(CREATE_NO_WINDOW)
                .output();
        }

        Ok(())
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Debugger/Profiler Attachment
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct DebuggerInfo {
    pub name: String,
    pub command: String,
    pub available: bool,
}

#[tauri::command]
async fn get_available_debuggers() -> Result<Vec<DebuggerInfo>, String> {
    let mut debuggers = Vec::new();

    // Windows debuggers
    debuggers.push(DebuggerInfo {
        name: "Visual Studio Debugger".to_string(),
        command: "vsjitdebugger".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "WinDbg".to_string(),
        command: "windbg".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "x64dbg".to_string(),
        command: "x64dbg".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "Process Monitor".to_string(),
        command: "procmon".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "Process Explorer".to_string(),
        command: "procexp".to_string(),
        available: true,
    });

    // Linux debuggers (via WSL)
    debuggers.push(DebuggerInfo {
        name: "gdb".to_string(),
        command: "wsl -e gdb -p".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "strace".to_string(),
        command: "wsl -e strace -p".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "ltrace".to_string(),
        command: "wsl -e ltrace -p".to_string(),
        available: true,
    });
    debuggers.push(DebuggerInfo {
        name: "perf top".to_string(),
        command: "wsl -e perf top -p".to_string(),
        available: true,
    });

    Ok(debuggers)
}

#[tauri::command]
async fn attach_debugger(pid: u32, debugger: String, _source: String) -> Result<(), String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let command = match debugger.as_str() {
            "vsjitdebugger" => {
                Command::new("vsjitdebugger")
                    .args(["-p", &pid.to_string()])
                    .spawn()
            }
            "windbg" => {
                Command::new("windbg")
                    .args(["-p", &pid.to_string()])
                    .spawn()
            }
            "x64dbg" => {
                Command::new("x64dbg")
                    .args(["-p", &pid.to_string()])
                    .spawn()
            }
            "procmon" => {
                Command::new("procmon")
                    .spawn()
            }
            "procexp" => {
                Command::new("procexp")
                    .spawn()
            }
            "gdb" => {
                Command::new("wsl")
                    .args(["-e", "sudo", "gdb", "-p", &pid.to_string()])
                    .spawn()
            }
            "strace" => {
                // Open new terminal with strace
                Command::new("wt")
                    .args(["wsl", "-e", "sudo", "strace", "-p", &pid.to_string()])
                    .spawn()
            }
            "ltrace" => {
                Command::new("wt")
                    .args(["wsl", "-e", "sudo", "ltrace", "-p", &pid.to_string()])
                    .spawn()
            }
            "perf" => {
                Command::new("wt")
                    .args(["wsl", "-e", "sudo", "perf", "top", "-p", &pid.to_string()])
                    .spawn()
            }
            _ => return Err(format!("Unknown debugger: {}", debugger)),
        };

        command.map_err(|e| format!("Failed to start debugger: {}", e))?;
        Ok(())
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Process Details (Open Files, etc.)
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessDetail {
    pub pid: u32,
    pub threads: Vec<ThreadInfo>,
    pub open_files: Vec<String>,
    pub environment: Vec<String>,
    pub modules: Vec<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ThreadInfo {
    pub tid: u32,
    pub state: String,
    pub cpu_percent: f32,
}

#[tauri::command]
async fn get_process_details(pid: u32, source: String) -> Result<ProcessDetail, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let mut detail = ProcessDetail {
            pid,
            threads: Vec::new(),
            open_files: Vec::new(),
            environment: Vec::new(),
            modules: Vec::new(),
        };

        #[cfg(windows)]
        if source == "windows" || source.is_empty() {
            // Get open handles via PowerShell (handle.exe alternative)
            if let Ok(output) = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    &format!(
                        r#"Get-Process -Id {} -ErrorAction SilentlyContinue | ForEach-Object {{ $_.Modules | Select-Object -First 50 | ForEach-Object {{ $_.FileName }} }}"#,
                        pid
                    )
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let trimmed = line.trim();
                        if !trimmed.is_empty() {
                            detail.modules.push(trimmed.to_string());
                        }
                    }
                }
            }

            // Get environment variables
            if let Ok(output) = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    &format!(
                        r#"
                        $p = Get-Process -Id {} -ErrorAction SilentlyContinue
                        if ($p) {{
                            $wmi = Get-WmiObject Win32_Process -Filter "ProcessId={}" -ErrorAction SilentlyContinue
                            if ($wmi) {{
                                $cmd = $wmi.CommandLine
                                if ($cmd) {{ Write-Output "CMD:$cmd" }}
                            }}
                        }}
                        "#,
                        pid, pid
                    )
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let trimmed = line.trim();
                        if !trimmed.is_empty() {
                            detail.environment.push(trimmed.to_string());
                        }
                    }
                }
            }

            // Get open files via handle output simulation
            if let Ok(output) = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    &format!(
                        r#"Get-Process -Id {} -ErrorAction SilentlyContinue | Select-Object -ExpandProperty Handles -ErrorAction SilentlyContinue"#,
                        pid
                    )
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    if let Ok(count) = stdout.trim().parse::<u32>() {
                        detail.open_files.push(format!("{} handles", count));
                    }
                }
            }
        }

        #[cfg(windows)]
        if source.starts_with("wsl") || source == "linux" {
            // Get open files via lsof
            if let Ok(output) = Command::new("wsl")
                .args(["-e", "lsof", "-p", &pid.to_string()])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines().skip(1).take(50) {
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 9 {
                            detail.open_files.push(parts[8].to_string());
                        }
                    }
                }
            }

            // Get environment variables
            if let Ok(output) = Command::new("wsl")
                .args(["-e", "cat", &format!("/proc/{}/environ", pid)])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for var in stdout.split('\0').take(50) {
                        if !var.is_empty() {
                            detail.environment.push(var.to_string());
                        }
                    }
                }
            }

            // Get loaded shared libraries
            if let Ok(output) = Command::new("wsl")
                .args(["-e", "cat", &format!("/proc/{}/maps", pid)])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let mut seen = std::collections::HashSet::new();
                    for line in stdout.lines() {
                        if let Some(path) = line.split_whitespace().last() {
                            if path.starts_with('/') && path.contains(".so") && !seen.contains(path) {
                                seen.insert(path.to_string());
                                detail.modules.push(path.to_string());
                                if detail.modules.len() >= 50 {
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(detail)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// CPU Affinity Control
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessAffinity {
    pub pid: u32,
    pub mask: u64,
    pub core_count: u32,
    pub cores: Vec<bool>,
}

#[tauri::command]
async fn get_process_affinity(pid: u32) -> Result<ProcessAffinity, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let core_count = num_cpus::get() as u32;

        #[cfg(windows)]
        {
            let output = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    &format!(
                        r#"(Get-Process -Id {} -ErrorAction SilentlyContinue).ProcessorAffinity.ToInt64()"#,
                        pid
                    )
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| format!("Failed to get affinity: {}", e))?;

            if output.status.success() {
                let stdout = String::from_utf8_lossy(&output.stdout);
                let mask: u64 = stdout.trim().parse().unwrap_or((1u64 << core_count) - 1);
                let mut cores = Vec::with_capacity(core_count as usize);
                for i in 0..core_count {
                    cores.push((mask & (1 << i)) != 0);
                }
                return Ok(ProcessAffinity { pid, mask, core_count, cores });
            }
        }

        // Default: all cores
        let mask = (1u64 << core_count) - 1;
        let cores = vec![true; core_count as usize];
        Ok(ProcessAffinity { pid, mask, core_count, cores })
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

#[tauri::command]
async fn set_process_affinity(pid: u32, mask: u64) -> Result<(), String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        #[cfg(windows)]
        {
            let output = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    &format!(
                        r#"$p = Get-Process -Id {} -ErrorAction SilentlyContinue; if ($p) {{ $p.ProcessorAffinity = {} }}"#,
                        pid, mask
                    )
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| format!("Failed to set affinity: {}", e))?;

            if !output.status.success() {
                let stderr = String::from_utf8_lossy(&output.stderr);
                return Err(format!("Failed to set affinity: {}", stderr));
            }
        }

        Ok(())
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// GPU Monitoring
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct GpuInfo {
    pub name: String,
    pub utilization: f32,
    pub memory_used_mb: u64,
    pub memory_total_mb: u64,
    pub temperature: Option<f32>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessGpuUsage {
    pub pid: u32,
    pub name: String,
    pub gpu_percent: f32,
    pub gpu_memory_mb: u64,
}

#[tauri::command]
async fn get_gpu_info() -> Result<Vec<GpuInfo>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let mut gpus = Vec::new();

        #[cfg(windows)]
        {
            // Try nvidia-smi first (for NVIDIA GPUs)
            if let Ok(output) = Command::new("nvidia-smi")
                .args([
                    "--query-gpu=name,utilization.gpu,memory.used,memory.total,temperature.gpu",
                    "--format=csv,noheader,nounits"
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let parts: Vec<&str> = line.split(',').map(|s| s.trim()).collect();
                        if parts.len() >= 5 {
                            gpus.push(GpuInfo {
                                name: parts[0].to_string(),
                                utilization: parts[1].parse().unwrap_or(0.0),
                                memory_used_mb: parts[2].parse().unwrap_or(0),
                                memory_total_mb: parts[3].parse().unwrap_or(0),
                                temperature: parts[4].parse().ok(),
                            });
                        }
                    }
                }
            }

            // If no NVIDIA GPU, try Windows Performance Counters
            if gpus.is_empty() {
                if let Ok(output) = Command::new("powershell")
                    .args([
                        "-NoProfile", "-Command",
                        r#"
                        Get-WmiObject Win32_VideoController | ForEach-Object {
                            $name = $_.Name
                            $vram = [math]::Round($_.AdapterRAM / 1MB, 0)
                            Write-Output "$name,$vram"
                        }
                        "#
                    ])
                    .creation_flags(CREATE_NO_WINDOW)
                    .output()
                {
                    if output.status.success() {
                        let stdout = String::from_utf8_lossy(&output.stdout);
                        for line in stdout.lines() {
                            let parts: Vec<&str> = line.split(',').collect();
                            if parts.len() >= 2 {
                                gpus.push(GpuInfo {
                                    name: parts[0].to_string(),
                                    utilization: 0.0, // Not available via WMI
                                    memory_used_mb: 0,
                                    memory_total_mb: parts[1].parse().unwrap_or(0),
                                    temperature: None,
                                });
                            }
                        }
                    }
                }
            }

            // Get GPU engine utilization via performance counters
            if let Ok(output) = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    r#"
                    $gpu = (Get-Counter '\GPU Engine(*engtype_3D)\Utilization Percentage' -ErrorAction SilentlyContinue).CounterSamples |
                        Where-Object { $_.CookedValue -gt 0 } |
                        Measure-Object -Property CookedValue -Sum
                    if ($gpu.Sum) { [math]::Min($gpu.Sum, 100) } else { 0 }
                    "#
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    if let Ok(util) = stdout.trim().parse::<f32>() {
                        if let Some(gpu) = gpus.first_mut() {
                            gpu.utilization = util;
                        }
                    }
                }
            }
        }

        Ok(gpus)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

#[tauri::command]
async fn get_gpu_processes() -> Result<Vec<ProcessGpuUsage>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let mut processes = Vec::new();

        #[cfg(windows)]
        {
            // Try nvidia-smi for per-process GPU usage
            if let Ok(output) = Command::new("nvidia-smi")
                .args([
                    "--query-compute-apps=pid,name,used_memory",
                    "--format=csv,noheader,nounits"
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let parts: Vec<&str> = line.split(',').map(|s| s.trim()).collect();
                        if parts.len() >= 3 {
                            if let Ok(pid) = parts[0].parse::<u32>() {
                                processes.push(ProcessGpuUsage {
                                    pid,
                                    name: parts[1].to_string(),
                                    gpu_percent: 0.0, // nvidia-smi doesn't give per-process %
                                    gpu_memory_mb: parts[2].parse().unwrap_or(0),
                                });
                            }
                        }
                    }
                }
            }

            // Windows GPU performance counters for per-process usage
            if let Ok(output) = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    r#"
                    Get-Counter '\GPU Engine(*)\Utilization Percentage' -ErrorAction SilentlyContinue |
                        ForEach-Object { $_.CounterSamples } |
                        Where-Object { $_.CookedValue -gt 0.1 } |
                        Group-Object { $_.InstanceName -replace '_.*', '' } |
                        ForEach-Object {
                            $pid = $_.Name -replace 'pid_', ''
                            $util = ($_.Group | Measure-Object -Property CookedValue -Sum).Sum
                            if ($pid -match '^\d+$') {
                                $proc = Get-Process -Id $pid -ErrorAction SilentlyContinue
                                $name = if ($proc) { $proc.ProcessName } else { 'Unknown' }
                                "$pid,$name,$util"
                            }
                        }
                    "#
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let parts: Vec<&str> = line.split(',').collect();
                        if parts.len() >= 3 {
                            if let Ok(pid) = parts[0].parse::<u32>() {
                                // Check if we already have this PID from nvidia-smi
                                if !processes.iter().any(|p| p.pid == pid) {
                                    processes.push(ProcessGpuUsage {
                                        pid,
                                        name: parts[1].to_string(),
                                        gpu_percent: parts[2].parse().unwrap_or(0.0),
                                        gpu_memory_mb: 0,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }

        Ok(processes)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Network I/O per Process
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessNetworkIo {
    pub pid: u32,
    pub name: String,
    pub bytes_sent: u64,
    pub bytes_recv: u64,
    pub connections: u32,
}

#[tauri::command]
async fn get_process_network_io() -> Result<Vec<ProcessNetworkIo>, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let mut processes = Vec::new();

        #[cfg(windows)]
        {
            // Get network stats per process using netstat + process correlation
            if let Ok(output) = Command::new("powershell")
                .args([
                    "-NoProfile", "-Command",
                    r#"
                    Get-NetTCPConnection -State Established -ErrorAction SilentlyContinue |
                        Group-Object OwningProcess |
                        ForEach-Object {
                            $pid = $_.Name
                            $count = $_.Count
                            $proc = Get-Process -Id $pid -ErrorAction SilentlyContinue
                            $name = if ($proc) { $proc.ProcessName } else { 'System' }
                            # Get I/O counters
                            $io = if ($proc) {
                                $proc | Select-Object -ExpandProperty IO -ErrorAction SilentlyContinue
                            }
                            $readBytes = if ($io) { $io.ReadBytes } else { 0 }
                            $writeBytes = if ($io) { $io.WriteBytes } else { 0 }
                            "$pid,$name,$count,$readBytes,$writeBytes"
                        }
                    "#
                ])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let parts: Vec<&str> = line.split(',').collect();
                        if parts.len() >= 5 {
                            if let Ok(pid) = parts[0].parse::<u32>() {
                                processes.push(ProcessNetworkIo {
                                    pid,
                                    name: parts[1].to_string(),
                                    connections: parts[2].parse().unwrap_or(0),
                                    bytes_recv: parts[3].parse().unwrap_or(0),
                                    bytes_sent: parts[4].parse().unwrap_or(0),
                                });
                            }
                        }
                    }
                }
            }
        }

        // Sort by connection count
        processes.sort_by(|a, b| b.connections.cmp(&a.connections));
        Ok(processes)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Network Tool Integration
// ============================================================================

#[tauri::command]
async fn launch_network_tool(tool: String, args: Vec<String>) -> Result<String, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        // Determine which tools need sudo/elevation
        let needs_sudo = matches!(tool.as_str(), "tcpdump" | "nmap");

        // Build the command to run in Windows Terminal with WSL
        let mut wsl_cmd = Vec::new();

        if needs_sudo {
            wsl_cmd.push("sudo".to_string());
        }

        wsl_cmd.push(tool.clone());
        wsl_cmd.extend(args.clone());

        // Build wt command: wt wsl -e <command>
        let mut wt_args = vec!["wsl".to_string(), "-e".to_string()];
        wt_args.extend(wsl_cmd);

        // Try to launch Windows Terminal
        let result = Command::new("wt")
            .args(&wt_args)
            .spawn();

        match result {
            Ok(_) => {
                if needs_sudo {
                    Ok(format!("Launched {} in Windows Terminal (requires sudo)", tool))
                } else {
                    Ok(format!("Launched {} in Windows Terminal", tool))
                }
            }
            Err(e) => {
                // Fallback: try launching directly in cmd
                let cmd_str = format!("wsl -e {}", wt_args[2..].join(" "));
                let _ = Command::new("cmd")
                    .args(["/c", "start", "cmd", "/k", &cmd_str])
                    .spawn();
                Err(format!("Windows Terminal not found, tried cmd fallback: {}", e))
            }
        }
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

#[tauri::command]
async fn whois_lookup(ip: String) -> Result<String, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        // Run whois via WSL
        let output = Command::new("wsl")
            .args(["-e", "whois", &ip])
            .creation_flags(CREATE_NO_WINDOW)
            .output()
            .map_err(|e| format!("Failed to run whois: {}", e))?;

        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            // Trim excessive blank lines and limit output
            let trimmed: String = stdout
                .lines()
                .filter(|line| !line.is_empty() || line.starts_with('%'))
                .take(100)
                .collect::<Vec<_>>()
                .join("\n");
            Ok(trimmed)
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            Err(format!("Whois failed: {}", stderr))
        }
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

// ============================================================================
// Process Explorer / Profiler
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MemoryRegion {
    pub address: String,
    pub size_kb: u64,
    pub region_type: String, // heap, stack, mapped, image, private
    pub protection: String,  // r, rw, rwx, etc.
    pub mapped_file: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessMemoryMap {
    pub pid: u32,
    pub total_virtual_kb: u64,
    pub total_private_kb: u64,
    pub regions: Vec<MemoryRegion>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct CoreUsage {
    pub core_id: u32,
    pub usage_percent: f32,
    pub top_process: Option<String>,
    pub top_pid: Option<u32>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct SystemHeatmap {
    pub cores: Vec<CoreUsage>,
    pub memory_used_percent: f32,
    pub memory_regions: Vec<MemoryBlock>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct MemoryBlock {
    pub start_percent: f32,  // 0-100 position in memory
    pub size_percent: f32,   // 0-100 size
    pub process_name: String,
    pub pid: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessTimelineSample {
    pub timestamp_ms: u64,
    pub cpu_percent: f32,
    pub memory_mb: f32,
    pub thread_count: u32,
    pub io_read_bytes: u64,
    pub io_write_bytes: u64,
    pub current_core: Option<u32>,
}

#[tauri::command]
async fn get_process_memory_map(pid: u32, source: String) -> Result<ProcessMemoryMap, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let mut map = ProcessMemoryMap {
            pid,
            total_virtual_kb: 0,
            total_private_kb: 0,
            regions: Vec::new(),
        };

        if source == "windows" {
            #[cfg(windows)]
            {
                // Use PowerShell to get memory regions (VirtualQueryEx equivalent)
                if let Ok(output) = Command::new("powershell")
                    .args([
                        "-NoProfile", "-Command",
                        &format!(r#"
                            $p = Get-Process -Id {} -ErrorAction SilentlyContinue
                            if ($p) {{
                                "$($p.VirtualMemorySize64/1024)|$($p.PrivateMemorySize64/1024)"
                                $p.Modules | Select-Object -First 20 | ForEach-Object {{
                                    "$($_.BaseAddress)|$($_.ModuleMemorySize/1024)|image|r-x|$($_.FileName)"
                                }}
                            }}
                        "#, pid)
                    ])
                    .creation_flags(CREATE_NO_WINDOW)
                    .output()
                {
                    if output.status.success() {
                        let stdout = String::from_utf8_lossy(&output.stdout);
                        let lines: Vec<&str> = stdout.lines().collect();

                        if let Some(first) = lines.first() {
                            let parts: Vec<&str> = first.split('|').collect();
                            map.total_virtual_kb = parts.first().and_then(|s| s.parse().ok()).unwrap_or(0);
                            map.total_private_kb = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
                        }

                        for line in lines.iter().skip(1) {
                            let parts: Vec<&str> = line.split('|').collect();
                            if parts.len() >= 5 {
                                map.regions.push(MemoryRegion {
                                    address: parts[0].to_string(),
                                    size_kb: parts[1].parse().unwrap_or(0),
                                    region_type: parts[2].to_string(),
                                    protection: parts[3].to_string(),
                                    mapped_file: Some(parts[4].to_string()),
                                });
                            }
                        }
                    }
                }
            }
        } else {
            // Linux: Parse /proc/[pid]/maps
            if let Ok(output) = Command::new("wsl")
                .args(["-e", "cat", &format!("/proc/{}/maps", pid)])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines().take(50) {
                        // Format: address perms offset dev inode pathname
                        let parts: Vec<&str> = line.split_whitespace().collect();
                        if parts.len() >= 5 {
                            let addr_range: Vec<&str> = parts[0].split('-').collect();
                            if addr_range.len() == 2 {
                                let start = u64::from_str_radix(addr_range[0], 16).unwrap_or(0);
                                let end = u64::from_str_radix(addr_range[1], 16).unwrap_or(0);
                                let size_kb = (end - start) / 1024;

                                let region_type = if parts.len() > 5 {
                                    let path = parts[5];
                                    if path.contains(".so") { "shared" }
                                    else if path.starts_with('/') { "mapped" }
                                    else if path == "[heap]" { "heap" }
                                    else if path == "[stack]" { "stack" }
                                    else { "private" }
                                } else {
                                    "private"
                                };

                                map.regions.push(MemoryRegion {
                                    address: parts[0].to_string(),
                                    size_kb,
                                    region_type: region_type.to_string(),
                                    protection: parts[1].to_string(),
                                    mapped_file: if parts.len() > 5 { Some(parts[5].to_string()) } else { None },
                                });
                                map.total_virtual_kb += size_kb;
                            }
                        }
                    }
                }
            }
        }

        Ok(map)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

#[tauri::command]
async fn get_system_heatmap() -> Result<SystemHeatmap, String> {
    use tokio::task::spawn_blocking;

    spawn_blocking(move || {
        let mut heatmap = SystemHeatmap {
            cores: Vec::new(),
            memory_used_percent: 0.0,
            memory_regions: Vec::new(),
        };

        // Get per-core CPU usage and top processes
        #[cfg(windows)]
        {
            // Get memory info
            if let Some(mem) = get_native_memory_info() {
                heatmap.memory_used_percent = mem.percent;
            }

            // Get per-core usage with top process
            if let Ok(output) = Command::new("wsl")
                .args(["-e", "bash", "-c", r#"
                    # Per-core CPU from /proc/stat
                    grep '^cpu[0-9]' /proc/stat | while read line; do
                        core=$(echo "$line" | awk '{print $1}' | sed 's/cpu//')
                        user=$(echo "$line" | awk '{print $2}')
                        nice=$(echo "$line" | awk '{print $3}')
                        system=$(echo "$line" | awk '{print $4}')
                        idle=$(echo "$line" | awk '{print $5}')
                        total=$((user + nice + system + idle))
                        used=$((user + nice + system))
                        if [ $total -gt 0 ]; then
                            pct=$((used * 100 / total))
                            echo "$core|$pct"
                        fi
                    done
                "#])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    for line in stdout.lines() {
                        let parts: Vec<&str> = line.split('|').collect();
                        if parts.len() >= 2 {
                            heatmap.cores.push(CoreUsage {
                                core_id: parts[0].parse().unwrap_or(0),
                                usage_percent: parts[1].parse().unwrap_or(0.0),
                                top_process: None,
                                top_pid: None,
                            });
                        }
                    }
                }
            }

            // Get top memory consumers for memory blocks visualization
            let processes = get_windows_processes();
            let total_mem: f32 = processes.iter().map(|p| p.memory_mb).sum();
            let mut sorted = processes.clone();
            sorted.sort_by(|a, b| b.memory_mb.partial_cmp(&a.memory_mb).unwrap_or(std::cmp::Ordering::Equal));

            let mut current_pos = 0.0f32;
            for proc in sorted.iter().take(10) {
                if proc.memory_mb > 0.0 && total_mem > 0.0 {
                    let size_pct = (proc.memory_mb / total_mem) * 100.0;
                    heatmap.memory_regions.push(MemoryBlock {
                        start_percent: current_pos,
                        size_percent: size_pct,
                        process_name: proc.short_name.clone(),
                        pid: proc.pid,
                    });
                    current_pos += size_pct;
                }
            }
        }

        Ok(heatmap)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

#[tauri::command]
async fn get_process_timeline_sample(pid: u32, source: String) -> Result<ProcessTimelineSample, String> {
    use tokio::task::spawn_blocking;
    use std::time::{SystemTime, UNIX_EPOCH};

    spawn_blocking(move || {
        let timestamp_ms = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_millis() as u64)
            .unwrap_or(0);

        let mut sample = ProcessTimelineSample {
            timestamp_ms,
            cpu_percent: 0.0,
            memory_mb: 0.0,
            thread_count: 0,
            io_read_bytes: 0,
            io_write_bytes: 0,
            current_core: None,
        };

        if source == "windows" {
            #[cfg(windows)]
            {
                // Get process info using native APIs
                unsafe {
                    if let Ok(handle) = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, false, pid) {
                        // Memory info
                        let mut mem_counters = PROCESS_MEMORY_COUNTERS::default();
                        mem_counters.cb = std::mem::size_of::<PROCESS_MEMORY_COUNTERS>() as u32;
                        if K32GetProcessMemoryInfo(handle, &mut mem_counters, mem_counters.cb).as_bool() {
                            sample.memory_mb = mem_counters.WorkingSetSize as f32 / 1024.0 / 1024.0;
                        }

                        // IO counters
                        let mut io_counters = IO_COUNTERS::default();
                        if GetProcessIoCounters(handle, &mut io_counters).is_ok() {
                            sample.io_read_bytes = io_counters.ReadTransferCount;
                            sample.io_write_bytes = io_counters.WriteTransferCount;
                        }

                        let _ = CloseHandle(handle);
                    }
                }

                // Get CPU and thread count via quick PowerShell
                if let Ok(output) = Command::new("powershell")
                    .args([
                        "-NoProfile", "-Command",
                        &format!("$p = Get-Process -Id {} -EA 0; if($p){{\"$($p.CPU)|$($p.Threads.Count)\"}}", pid)
                    ])
                    .creation_flags(CREATE_NO_WINDOW)
                    .output()
                {
                    if output.status.success() {
                        let stdout = String::from_utf8_lossy(&output.stdout);
                        let parts: Vec<&str> = stdout.trim().split('|').collect();
                        // CPU is total CPU time, not percent - would need delta calculation
                        sample.thread_count = parts.get(1).and_then(|s| s.parse().ok()).unwrap_or(0);
                    }
                }
            }
        } else {
            // Linux
            if let Ok(output) = Command::new("wsl")
                .args(["-e", "bash", "-c", &format!(r#"
                    if [ -d /proc/{} ]; then
                        stat=$(cat /proc/{}/stat 2>/dev/null)
                        rss=$(echo "$stat" | awk '{{print $24}}')
                        threads=$(echo "$stat" | awk '{{print $20}}')
                        cpu=$(echo "$stat" | awk '{{print $14 + $15}}')
                        core=$(echo "$stat" | awk '{{print $39}}')
                        io=$(cat /proc/{}/io 2>/dev/null | grep -E '^(read_bytes|write_bytes)' | awk '{{print $2}}' | tr '\n' '|')
                        echo "$rss|$threads|$cpu|$core|$io"
                    fi
                "#, pid, pid, pid)])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
            {
                if output.status.success() {
                    let stdout = String::from_utf8_lossy(&output.stdout);
                    let parts: Vec<&str> = stdout.trim().split('|').collect();
                    if parts.len() >= 4 {
                        let rss_pages: u64 = parts[0].parse().unwrap_or(0);
                        sample.memory_mb = (rss_pages * 4) as f32 / 1024.0; // 4KB pages to MB
                        sample.thread_count = parts[1].parse().unwrap_or(0);
                        sample.current_core = parts[3].parse().ok();
                        if parts.len() >= 6 {
                            sample.io_read_bytes = parts[4].parse().unwrap_or(0);
                            sample.io_write_bytes = parts[5].trim_end_matches('|').parse().unwrap_or(0);
                        }
                    }
                }
            }
        }

        Ok(sample)
    })
    .await
    .map_err(|e| format!("Task failed: {}", e))?
}

#[tauri::command]
fn update_tray_tooltip(
    tray_state: State<TrayState>,
    cpu_percent: f32,
    memory_percent: f32,
    memory_used_mb: u64,
) -> Result<(), String> {
    if let Some(ref tray) = *tray_state.0.lock() {
        let tooltip = format!(
            "WSL Task Manager\nCPU: {:.1}%\nMem: {:.0}% ({} MiB)",
            cpu_percent, memory_percent, memory_used_mb
        );
        tray.set_tooltip(Some(&tooltip))
            .map_err(|e| format!("Failed to set tooltip: {}", e))?;
    }
    Ok(())
}

// ============================================================================
// Tray Setup
// ============================================================================

fn setup_tray(app: &tauri::App) -> Result<TrayIcon, Box<dyn std::error::Error>> {
    let show_i = MenuItem::with_id(app, "show", "Show Window", true, None::<&str>)?;
    let refresh_i = MenuItem::with_id(app, "refresh", "Refresh", true, None::<&str>)?;
    let kill_node_i = MenuItem::with_id(app, "kill_node", "Kill All Node", true, None::<&str>)?;
    let kill_php_i = MenuItem::with_id(app, "kill_php", "Kill All PHP", true, None::<&str>)?;
    let quit_i = MenuItem::with_id(app, "quit", "Exit", true, None::<&str>)?;

    let menu = Menu::with_items(
        app,
        &[&show_i, &refresh_i, &kill_node_i, &kill_php_i, &quit_i],
    )?;

    let tray = TrayIconBuilder::new()
        .icon(app.default_window_icon().unwrap().clone())
        .menu(&menu)
        .show_menu_on_left_click(false) // Left click shows window, right click shows menu
        .tooltip("WSL Task Manager\nLoading...")
        .on_menu_event(|app, event| match event.id.as_ref() {
            "show" => {
                if let Some(window) = app.get_webview_window("main") {
                    let _ = window.show();
                    let _ = window.set_focus();
                }
            }
            "refresh" => {
                if let Some(window) = app.get_webview_window("main") {
                    let _ = window.emit("refresh", ());
                }
            }
            "kill_node" => {
                let _ = run_wsl(&["-e", "pkill", "-9", "-f", "node"]);
                if let Some(window) = app.get_webview_window("main") {
                    let _ = window.emit("refresh", ());
                }
            }
            "kill_php" => {
                let _ = run_wsl(&["-e", "pkill", "-9", "-f", "php"]);
                if let Some(window) = app.get_webview_window("main") {
                    let _ = window.emit("refresh", ());
                }
            }
            "quit" => {
                app.exit(0);
            }
            _ => {}
        })
        .on_tray_icon_event(|tray, event| {
            if let TrayIconEvent::Click {
                button: MouseButton::Left,
                button_state: MouseButtonState::Up,
                ..
            } = event
            {
                let app = tray.app_handle();
                if let Some(window) = app.get_webview_window("main") {
                    // Toggle visibility: show if hidden, hide if visible
                    if window.is_visible().unwrap_or(false) {
                        let _ = window.hide();
                    } else {
                        let _ = window.show();
                        let _ = window.set_focus();
                    }
                }
            }
        })
        .build(app)?;

    Ok(tray)
}

// ============================================================================
// Main Entry
// ============================================================================

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    let tray_state = TrayState(Arc::new(Mutex::new(None)));
    let tray_state_clone = tray_state.0.clone();
    let shell_state = ShellState(Arc::new(Mutex::new(None)));
    let distro_state = DistroState(Arc::new(Mutex::new("default".to_string())));
    let cache_state = CacheState(Arc::new(Mutex::new(CachedData::default())));

    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .manage(tray_state)
        .manage(shell_state)
        .manage(cache_state)
        .manage(distro_state)
        .invoke_handler(tauri::generate_handler![
            get_stats,
            get_stats_for_source,
            get_overview,
            get_system_tree,
            refresh_system_tree,
            kill_process,
            kill_tree,
            kill_pattern,
            update_tray_tooltip,
            list_distros,
            list_sources,
            get_current_distro,
            set_distro,
            get_windows_services,
            get_linux_services,
            get_network_connections,
            get_startup_apps,
            get_cpu_cores,
            set_process_priority,
            get_available_debuggers,
            attach_debugger,
            get_process_details,
            get_process_affinity,
            set_process_affinity,
            get_gpu_info,
            get_gpu_processes,
            get_process_network_io,
            launch_network_tool,
            whois_lookup,
            get_process_memory_map,
            get_system_heatmap,
            get_process_timeline_sample
        ])
        .setup(move |app| {
            // Setup tray
            let tray = setup_tray(app)?;
            *tray_state_clone.lock() = Some(tray);

            // Apply Mica effect on Windows
            #[cfg(windows)]
            if let Some(window) = app.get_webview_window("main") {
                let _ = apply_mica(&window, Some(true)); // dark mode
            }

            Ok(())
        })
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
