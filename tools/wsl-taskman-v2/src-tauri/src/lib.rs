// TODO: Re-enable MCP module once rmcp API stabilizes
// The rmcp 0.12 API has significant changes from docs
// pub mod mcp;

use parking_lot::Mutex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};
use std::sync::Arc;

#[cfg(windows)]
use std::os::windows::process::CommandExt;

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
        OpenProcess, SetPriorityClass, TerminateProcess,
        PROCESS_QUERY_INFORMATION, PROCESS_TERMINATE, PROCESS_VM_READ,
        ABOVE_NORMAL_PRIORITY_CLASS, BELOW_NORMAL_PRIORITY_CLASS, HIGH_PRIORITY_CLASS,
        IDLE_PRIORITY_CLASS, NORMAL_PRIORITY_CLASS, REALTIME_PRIORITY_CLASS,
    },
};

#[cfg(windows)]
const CREATE_NO_WINDOW: u32 = 0x08000000;

// ============================================================================
// Types - Match frontend definitions
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Process {
    pub pid: u32,
    pub name: String,
    pub short_name: String,
    pub command: String,
    pub state: String,
    pub user: String,
    pub cpu_percent: f32,
    pub memory_mb: f32,
    pub threads: u32,
    pub parent_pid: Option<u32>,
    pub source: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SystemStats {
    pub cpu_percent: f32,
    pub load_avg: [f32; 3],
    pub memory: MemoryInfo,
    pub uptime: u64,
    pub hostname: String,
    pub kernel: String,
}

#[derive(Clone, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MemoryInfo {
    pub total_mb: u64,
    pub used_mb: u64,
    pub available_mb: u64,
    pub percent: f32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CoreUsage {
    pub index: u32,
    pub usage: f32,
    pub top_process: Option<String>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct GpuInfo {
    pub name: String,
    pub utilization_percent: f32,
    pub memory_used_mb: u64,
    pub memory_total_mb: u64,
    pub temperature: Option<u32>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SystemSnapshot {
    pub processes: Vec<Process>,
    pub stats: SystemStats,
    pub cores: Vec<CoreUsage>,
    pub gpu: Option<GpuInfo>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct NetworkConnection {
    pub id: String,
    pub local_addr: String,
    pub local_port: u16,
    pub remote_addr: String,
    pub remote_port: u16,
    pub state: String,
    pub protocol: String,
    pub pid: u32,
    pub process_name: String,
    pub source: String,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Service {
    pub name: String,
    pub display_name: String,
    pub status: String,
    pub start_type: String,
    pub source: String,
}

// ============================================================================
// Deep System Visualization Types
// ============================================================================

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CacheInfo {
    pub level: u8,           // L1, L2, L3
    pub cache_type: String,  // Data, Instruction, Unified
    pub size_kb: u32,
    pub line_size: u32,
    pub associativity: u32,
    pub sets: u32,
    pub shared_by_cores: u32,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CpuTopology {
    pub vendor: String,
    pub brand: String,
    pub physical_cores: u32,
    pub logical_cores: u32,
    pub caches: Vec<CacheInfo>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MemoryFlow {
    pub page_faults_per_sec: f64,
    pub cache_bytes: u64,
    pub cache_faults_per_sec: f64,
    pub commit_bytes: u64,
    pub available_bytes: u64,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DiskFlow {
    pub name: String,
    pub read_bytes_per_sec: u64,
    pub write_bytes_per_sec: u64,
    pub queue_length: u32,
}

// ============================================================================
// State Management
// ============================================================================

pub struct WslShell {
    _child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
}

impl WslShell {
    fn new() -> Result<Self, String> {
        let mut cmd = Command::new("wsl");
        cmd.args(["-e", "bash", "--norc", "--noprofile"])
            .stdin(Stdio::piped())
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
        })
    }

    fn run(&mut self, command: &str) -> Result<String, String> {
        let marker = format!("__END_{}__", std::process::id());
        let full_cmd = format!("{}\necho '{}'\n", command, marker);

        self.stdin.write_all(full_cmd.as_bytes()).map_err(|e| e.to_string())?;
        self.stdin.flush().map_err(|e| e.to_string())?;

        let mut output = String::new();
        let mut line = String::new();

        loop {
            line.clear();
            match self.stdout.read_line(&mut line) {
                Ok(0) => break,
                Ok(_) => {
                    if line.trim() == marker {
                        break;
                    }
                    output.push_str(&line);
                }
                Err(e) => return Err(e.to_string()),
            }
        }

        Ok(output)
    }
}

pub struct ShellState(pub Arc<Mutex<Option<WslShell>>>);

// CPU tracking for usage calculation
pub struct CpuState(pub Arc<Mutex<HashMap<u32, (u64, u64)>>>); // pid -> (kernel_time, user_time)

// ============================================================================
// Native Windows APIs
// ============================================================================

#[cfg(windows)]
pub fn get_native_memory() -> MemoryInfo {
    unsafe {
        let mut mem_status = MEMORYSTATUSEX {
            dwLength: std::mem::size_of::<MEMORYSTATUSEX>() as u32,
            ..Default::default()
        };
        if GlobalMemoryStatusEx(&mut mem_status).is_ok() {
            let total_mb = (mem_status.ullTotalPhys / 1024 / 1024) as u64;
            let available_mb = (mem_status.ullAvailPhys / 1024 / 1024) as u64;
            let used_mb = total_mb.saturating_sub(available_mb);
            let percent = if total_mb > 0 { used_mb as f32 / total_mb as f32 * 100.0 } else { 0.0 };
            MemoryInfo { total_mb, used_mb, available_mb, percent }
        } else {
            MemoryInfo::default()
        }
    }
}

#[cfg(windows)]
pub fn get_windows_processes() -> Vec<Process> {
    use std::mem::size_of;
    let mut processes = Vec::with_capacity(500);

    unsafe {
        let snapshot = match CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0) {
            Ok(h) => h,
            Err(_) => return processes,
        };

        let mut entry = PROCESSENTRY32 {
            dwSize: size_of::<PROCESSENTRY32>() as u32,
            ..Default::default()
        };

        if Process32First(snapshot, &mut entry).is_ok() {
            loop {
                let pid = entry.th32ProcessID;
                let ppid = entry.th32ParentProcessID;
                let threads = entry.cntThreads;

                if pid == 0 {
                    if Process32Next(snapshot, &mut entry).is_err() { break; }
                    continue;
                }

                let name_bytes: Vec<u8> = entry.szExeFile
                    .iter()
                    .take_while(|&&b| b != 0)
                    .map(|&b| b as u8)
                    .collect();
                let name = String::from_utf8_lossy(&name_bytes).to_string();
                let short_name = name.trim_end_matches(".exe").to_string();

                let mut memory_mb = 0.0f32;

                if let Ok(proc_handle) = OpenProcess(PROCESS_QUERY_INFORMATION | PROCESS_VM_READ, false, pid) {
                    let mut mem_counters = PROCESS_MEMORY_COUNTERS::default();
                    if K32GetProcessMemoryInfo(
                        proc_handle,
                        &mut mem_counters,
                        size_of::<PROCESS_MEMORY_COUNTERS>() as u32,
                    ).as_bool() {
                        memory_mb = mem_counters.WorkingSetSize as f32 / (1024.0 * 1024.0);
                    }
                    let _ = CloseHandle(proc_handle);
                }

                processes.push(Process {
                    pid,
                    name: name.clone(),
                    short_name,
                    command: name,
                    state: "Running".to_string(),
                    user: String::new(),
                    cpu_percent: 0.0,
                    memory_mb,
                    threads,
                    parent_pid: if ppid > 0 { Some(ppid) } else { None },
                    source: "windows".to_string(),
                });

                if Process32Next(snapshot, &mut entry).is_err() { break; }
            }
        }

        let _ = CloseHandle(snapshot);
    }

    processes
}

// ============================================================================
// Tauri Commands
// ============================================================================

#[tauri::command]
async fn get_system_snapshot(_source: Option<String>) -> Result<SystemSnapshot, String> {
    let processes = get_windows_processes();

    let memory = get_native_memory();

    // Calculate total CPU from process data (simplified)
    let total_cpu: f32 = processes.iter().map(|p| p.cpu_percent).sum();
    let cpu_percent = total_cpu.min(100.0);

    let stats = SystemStats {
        cpu_percent,
        load_avg: [0.0, 0.0, 0.0],
        memory,
        uptime: 0,
        hostname: hostname::get().map(|h| h.to_string_lossy().to_string()).unwrap_or_default(),
        kernel: "Windows".to_string(),
    };

    // Get per-core usage (simplified - would need proper implementation)
    let num_cores = num_cpus::get();
    let cores: Vec<CoreUsage> = (0..num_cores)
        .map(|i| CoreUsage {
            index: i as u32,
            usage: cpu_percent / num_cores as f32, // Distribute evenly for now
            top_process: None,
        })
        .collect();

    Ok(SystemSnapshot {
        processes,
        stats,
        cores,
        gpu: None, // Would need nvidia-smi or similar
    })
}

#[tauri::command]
async fn get_sources() -> Result<Vec<String>, String> {
    let mut sources = vec!["windows".to_string()];

    // Check for WSL distros
    #[cfg(windows)]
    {
        let output = Command::new("wsl")
            .args(["--list", "--quiet"])
            .creation_flags(CREATE_NO_WINDOW)
            .output();

        if let Ok(out) = output {
            let text = String::from_utf8_lossy(&out.stdout);
            for line in text.lines() {
                let distro = line.trim().trim_start_matches('\u{feff}');
                if !distro.is_empty() {
                    sources.push(format!("wsl:{}", distro));
                }
            }
        }
    }

    Ok(sources)
}

/// Internal implementation for killing processes (used by both Tauri and MCP)
pub async fn kill_process_impl(pid: u32, tree: bool) -> Result<(), String> {
    #[cfg(windows)]
    {
        if tree {
            Command::new("taskkill")
                .args(["/F", "/T", "/PID", &pid.to_string()])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| e.to_string())?;
        } else {
            unsafe {
                if let Ok(handle) = OpenProcess(PROCESS_TERMINATE, false, pid) {
                    let _ = TerminateProcess(handle, 1);
                    let _ = CloseHandle(handle);
                }
            }
        }
    }
    Ok(())
}

#[tauri::command]
async fn kill_process(pid: u32, tree: bool) -> Result<(), String> {
    kill_process_impl(pid, tree).await
}

/// Internal implementation for setting process priority (used by both Tauri and MCP)
pub async fn set_priority_impl(pid: u32, priority: &str) -> Result<(), String> {
    #[cfg(windows)]
    unsafe {
        if let Ok(handle) = OpenProcess(PROCESS_QUERY_INFORMATION | windows::Win32::System::Threading::PROCESS_SET_INFORMATION, false, pid) {
            let class = match priority {
                "realtime" => REALTIME_PRIORITY_CLASS,
                "high" => HIGH_PRIORITY_CLASS,
                "above_normal" => ABOVE_NORMAL_PRIORITY_CLASS,
                "normal" => NORMAL_PRIORITY_CLASS,
                "below_normal" => BELOW_NORMAL_PRIORITY_CLASS,
                "idle" => IDLE_PRIORITY_CLASS,
                _ => NORMAL_PRIORITY_CLASS,
            };
            let _ = SetPriorityClass(handle, class);
            let _ = CloseHandle(handle);
        }
    }
    Ok(())
}

#[tauri::command]
async fn set_priority(pid: u32, priority: String) -> Result<(), String> {
    set_priority_impl(pid, &priority).await
}

#[tauri::command]
async fn get_network_connections() -> Result<Vec<NetworkConnection>, String> {
    let mut connections = Vec::new();

    #[cfg(windows)]
    {
        let output = Command::new("powershell")
            .args(["-Command", "Get-NetTCPConnection | Select-Object LocalAddress,LocalPort,RemoteAddress,RemotePort,State,OwningProcess | ConvertTo-Json"])
            .creation_flags(CREATE_NO_WINDOW)
            .output()
            .map_err(|e| e.to_string())?;

        if let Ok(json) = serde_json::from_slice::<serde_json::Value>(&output.stdout) {
            let items = if json.is_array() { json.as_array().unwrap().clone() } else { vec![json] };

            for (i, item) in items.iter().enumerate() {
                connections.push(NetworkConnection {
                    id: format!("tcp-{}", i),
                    local_addr: item["LocalAddress"].as_str().unwrap_or("").to_string(),
                    local_port: item["LocalPort"].as_u64().unwrap_or(0) as u16,
                    remote_addr: item["RemoteAddress"].as_str().unwrap_or("").to_string(),
                    remote_port: item["RemotePort"].as_u64().unwrap_or(0) as u16,
                    state: item["State"].as_str().or_else(|| item["State"].as_u64().map(|_| "Unknown")).unwrap_or("Unknown").to_string(),
                    protocol: "tcp".to_string(),
                    pid: item["OwningProcess"].as_u64().unwrap_or(0) as u32,
                    process_name: String::new(),
                    source: "windows".to_string(),
                });
            }
        }
    }

    Ok(connections)
}

#[tauri::command]
async fn get_services(source: String) -> Result<Vec<Service>, String> {
    let mut services = Vec::new();

    if source == "windows" {
        #[cfg(windows)]
        {
            let output = Command::new("powershell")
                .args(["-Command", "Get-Service | Select-Object Name,DisplayName,Status,StartType | ConvertTo-Json"])
                .creation_flags(CREATE_NO_WINDOW)
                .output()
                .map_err(|e| e.to_string())?;

            if let Ok(json) = serde_json::from_slice::<serde_json::Value>(&output.stdout) {
                let items = if json.is_array() { json.as_array().unwrap().clone() } else { vec![json] };

                for item in items {
                    services.push(Service {
                        name: item["Name"].as_str().unwrap_or("").to_string(),
                        display_name: item["DisplayName"].as_str().unwrap_or("").to_string(),
                        status: match item["Status"].as_u64() {
                            Some(4) => "running".to_string(),
                            Some(1) => "stopped".to_string(),
                            _ => "unknown".to_string(),
                        },
                        start_type: match item["StartType"].as_u64() {
                            Some(2) => "Automatic".to_string(),
                            Some(3) => "Manual".to_string(),
                            Some(4) => "Disabled".to_string(),
                            _ => "Unknown".to_string(),
                        },
                        source: "windows".to_string(),
                    });
                }
            }
        }
    }

    Ok(services)
}

#[tauri::command]
async fn whois_lookup(ip: String) -> Result<String, String> {
    let output = Command::new("wsl")
        .args(["-e", "whois", &ip])
        .output()
        .map_err(|e| e.to_string())?;

    Ok(String::from_utf8_lossy(&output.stdout).to_string())
}

#[tauri::command]
async fn launch_tool(tool: String, args: Vec<String>) -> Result<(), String> {
    #[cfg(windows)]
    {
        let mut cmd = Command::new("wt");
        cmd.args(["--", &tool]).args(&args);
        cmd.spawn().map_err(|e| e.to_string())?;
    }
    Ok(())
}

// ============================================================================
// Deep System Visualization Commands
// ============================================================================

#[tauri::command]
async fn get_cache_topology() -> Result<CpuTopology, String> {
    use raw_cpuid::CpuId;

    let cpuid = CpuId::new();

    // Get vendor and brand
    let vendor = cpuid.get_vendor_info()
        .map(|v| v.as_str().to_string())
        .unwrap_or_else(|| "Unknown".to_string());

    let brand = cpuid.get_processor_brand_string()
        .map(|b| b.as_str().trim().to_string())
        .unwrap_or_else(|| "Unknown CPU".to_string());

    // Get core counts
    let logical_cores = num_cpus::get() as u32;
    let physical_cores = num_cpus::get_physical() as u32;

    // Get cache info from CPUID
    let mut caches = Vec::new();

    if let Some(cache_params) = cpuid.get_cache_parameters() {
        for cache in cache_params {
            let level = cache.level();
            let cache_type = match cache.cache_type() {
                raw_cpuid::CacheType::Data => "Data",
                raw_cpuid::CacheType::Instruction => "Instruction",
                raw_cpuid::CacheType::Unified => "Unified",
                _ => "Unknown",
            }.to_string();

            let line_size = cache.coherency_line_size() as u32;
            let associativity = cache.associativity() as u32;
            let sets = cache.sets() as u32;
            let partitions = cache.physical_line_partitions() as u32;

            // Calculate cache size: sets * associativity * line_size
            let size_bytes = sets * associativity * line_size * partitions;
            let size_kb = size_bytes / 1024;

            // How many logical processors share this cache
            let shared_by = cache.max_cores_for_cache() as u32;

            caches.push(CacheInfo {
                level,
                cache_type,
                size_kb,
                line_size,
                associativity,
                sets,
                shared_by_cores: shared_by,
            });
        }
    }

    // Sort caches by level and type
    caches.sort_by(|a, b| {
        a.level.cmp(&b.level).then_with(|| a.cache_type.cmp(&b.cache_type))
    });

    Ok(CpuTopology {
        vendor,
        brand,
        physical_cores,
        logical_cores,
        caches,
    })
}

#[tauri::command]
async fn get_memory_flow() -> Result<MemoryFlow, String> {
    // Use Windows Performance Counters via PowerShell for now
    // This is simpler than the full PDH API and works reliably
    #[cfg(windows)]
    {
        let output = Command::new("powershell")
            .args(["-Command", r#"
$counters = @(
    '\Memory\Page Faults/sec',
    '\Memory\Cache Bytes',
    '\Memory\Cache Faults/sec',
    '\Memory\Committed Bytes',
    '\Memory\Available Bytes'
)
$data = Get-Counter -Counter $counters -ErrorAction SilentlyContinue |
    Select-Object -ExpandProperty CounterSamples |
    Select-Object Path, CookedValue
$data | ConvertTo-Json
"#])
            .creation_flags(CREATE_NO_WINDOW)
            .output()
            .map_err(|e| e.to_string())?;

        if let Ok(json) = serde_json::from_slice::<serde_json::Value>(&output.stdout) {
            let items = if json.is_array() { json.as_array().unwrap().clone() } else { vec![json] };

            let mut page_faults = 0.0;
            let mut cache_bytes = 0u64;
            let mut cache_faults = 0.0;
            let mut commit_bytes = 0u64;
            let mut available_bytes = 0u64;

            for item in items {
                let path = item["Path"].as_str().unwrap_or("").to_lowercase();
                let value = item["CookedValue"].as_f64().unwrap_or(0.0);

                if path.contains("page faults/sec") && !path.contains("cache") {
                    page_faults = value;
                } else if path.contains("cache bytes") && !path.contains("faults") {
                    cache_bytes = value as u64;
                } else if path.contains("cache faults/sec") {
                    cache_faults = value;
                } else if path.contains("committed bytes") {
                    commit_bytes = value as u64;
                } else if path.contains("available bytes") {
                    available_bytes = value as u64;
                }
            }

            return Ok(MemoryFlow {
                page_faults_per_sec: page_faults,
                cache_bytes,
                cache_faults_per_sec: cache_faults,
                commit_bytes,
                available_bytes,
            });
        }
    }

    Err("Failed to get memory flow data".to_string())
}

#[tauri::command]
async fn get_disk_flow() -> Result<Vec<DiskFlow>, String> {
    #[cfg(windows)]
    {
        let output = Command::new("powershell")
            .args(["-Command", r#"
Get-Counter -Counter '\PhysicalDisk(*)\Disk Read Bytes/sec', '\PhysicalDisk(*)\Disk Write Bytes/sec', '\PhysicalDisk(*)\Current Disk Queue Length' -ErrorAction SilentlyContinue |
    Select-Object -ExpandProperty CounterSamples |
    Select-Object Path, InstanceName, CookedValue |
    ConvertTo-Json
"#])
            .creation_flags(CREATE_NO_WINDOW)
            .output()
            .map_err(|e| e.to_string())?;

        if let Ok(json) = serde_json::from_slice::<serde_json::Value>(&output.stdout) {
            let items = if json.is_array() { json.as_array().unwrap().clone() } else { vec![json] };

            let mut disk_map: HashMap<String, DiskFlow> = HashMap::new();

            for item in items {
                let path = item["Path"].as_str().unwrap_or("").to_lowercase();
                let instance = item["InstanceName"].as_str().unwrap_or("").to_string();
                let value = item["CookedValue"].as_f64().unwrap_or(0.0);

                // Skip the _total instance
                if instance == "_total" || instance.is_empty() {
                    continue;
                }

                let entry = disk_map.entry(instance.clone()).or_insert_with(|| DiskFlow {
                    name: instance,
                    read_bytes_per_sec: 0,
                    write_bytes_per_sec: 0,
                    queue_length: 0,
                });

                if path.contains("read bytes") {
                    entry.read_bytes_per_sec = value as u64;
                } else if path.contains("write bytes") {
                    entry.write_bytes_per_sec = value as u64;
                } else if path.contains("queue length") {
                    entry.queue_length = value as u32;
                }
            }

            return Ok(disk_map.into_values().collect());
        }
    }

    Err("Failed to get disk flow data".to_string())
}

// ============================================================================
// App Entry Point
// ============================================================================

use tauri::{
    tray::{MouseButton, MouseButtonState, TrayIconBuilder, TrayIconEvent},
    Manager,
};

#[cfg_attr(mobile, tauri::mobile_entry_point)]
pub fn run() {
    tauri::Builder::default()
        .plugin(tauri_plugin_opener::init())
        .manage(ShellState(Arc::new(Mutex::new(None))))
        .manage(CpuState(Arc::new(Mutex::new(HashMap::new()))))
        .setup(|app| {
            // Create tray icon for instant show/hide
            let _tray = TrayIconBuilder::new()
                .icon(app.default_window_icon().unwrap().clone())
                .tooltip("WSL Task Manager")
                .on_tray_icon_event(|tray, event| {
                    if let TrayIconEvent::Click {
                        button: MouseButton::Left,
                        button_state: MouseButtonState::Up,
                        ..
                    } = event
                    {
                        let app = tray.app_handle();
                        if let Some(window) = app.get_webview_window("main") {
                            // Toggle visibility for instant switching
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

            Ok(())
        })
        .on_window_event(|window, event| {
            // Hide to tray instead of closing for instant re-show
            if let tauri::WindowEvent::CloseRequested { api, .. } = event {
                let _ = window.hide();
                api.prevent_close();
            }
        })
        .invoke_handler(tauri::generate_handler![
            get_system_snapshot,
            get_sources,
            kill_process,
            set_priority,
            get_network_connections,
            get_services,
            whois_lookup,
            launch_tool,
            get_cache_topology,
            get_memory_flow,
            get_disk_flow,
        ])
        .run(tauri::generate_context!())
        .expect("error while running tauri application");
}
