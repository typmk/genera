//! MCP Server for exposing Task Manager capabilities to AI assistants
//!
//! This module implements the Model Context Protocol server that allows
//! AI assistants like Claude to query process information, kill processes,
//! and manage system resources.

use rmcp::{
    model::*,
    service::{RequestContext, RoleServer},
    ServerHandler, ServiceExt,
};
use serde_json::json;
use std::sync::Arc;
use tokio::sync::Mutex;

use crate::{get_native_memory, get_windows_processes, Process, SystemStats};

/// MCP Server that exposes Task Manager capabilities
#[derive(Clone)]
pub struct TaskManagerServer {
    cache: Arc<Mutex<Option<CachedData>>>,
}

struct CachedData {
    processes: Vec<Process>,
    stats: SystemStats,
    timestamp: std::time::Instant,
}

impl TaskManagerServer {
    pub fn new() -> Self {
        Self {
            cache: Arc::new(Mutex::new(None)),
        }
    }

    async fn refresh_cache(&self) -> (Vec<Process>, SystemStats) {
        let mut cache = self.cache.lock().await;

        // Use cached data if less than 1 second old
        if let Some(ref cached) = *cache {
            if cached.timestamp.elapsed().as_secs() < 1 {
                return (cached.processes.clone(), cached.stats.clone());
            }
        }

        let processes = get_windows_processes();
        let memory = get_native_memory();

        let total_cpu: f32 = processes.iter().map(|p| p.cpu_percent).sum();

        let stats = SystemStats {
            cpu_percent: total_cpu.min(100.0),
            load_avg: [0.0, 0.0, 0.0],
            memory,
            uptime: 0,
            hostname: hostname::get()
                .map(|h| h.to_string_lossy().to_string())
                .unwrap_or_default(),
            kernel: "Windows".to_string(),
        };

        *cache = Some(CachedData {
            processes: processes.clone(),
            stats: stats.clone(),
            timestamp: std::time::Instant::now(),
        });

        (processes, stats)
    }
}

fn make_tool(name: &str, description: &str, schema: serde_json::Value) -> Tool {
    Tool {
        name: name.into(),
        description: Some(description.into()),
        input_schema: schema.as_object().cloned().unwrap().into(),
        annotations: None,
    }
}

impl ServerHandler for TaskManagerServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            name: "wsl-taskman".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            instructions: Some(
                "WSL Task Manager MCP Server - Query and manage Windows/WSL processes. \
                 Available tools: list_processes, top_cpu, top_memory, get_process, \
                 kill_process, set_priority, search_processes, memory_summary, system_stats"
                    .into(),
            ),
        }
    }

    fn list_tools(
        &self,
        _request: ListToolsRequest,
        _context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<ListToolsResult, rmcp::ErrorData>> + Send + '_ {
        async move {
            let tools = vec![
                make_tool(
                    "list_processes",
                    "List running processes with optional filtering",
                    json!({
                        "type": "object",
                        "properties": {
                            "name": { "type": "string", "description": "Filter by process name (substring match)" },
                            "min_memory_mb": { "type": "number", "description": "Minimum memory usage in MB" },
                            "limit": { "type": "integer", "description": "Maximum results to return" }
                        }
                    }),
                ),
                make_tool(
                    "top_cpu",
                    "Get top processes by CPU usage",
                    json!({
                        "type": "object",
                        "properties": {
                            "limit": { "type": "integer", "description": "Number of results (default 10)" }
                        }
                    }),
                ),
                make_tool(
                    "top_memory",
                    "Get top processes by memory usage",
                    json!({
                        "type": "object",
                        "properties": {
                            "limit": { "type": "integer", "description": "Number of results (default 10)" }
                        }
                    }),
                ),
                make_tool(
                    "get_process",
                    "Get details for a specific process by PID",
                    json!({
                        "type": "object",
                        "properties": {
                            "pid": { "type": "integer", "description": "Process ID" }
                        },
                        "required": ["pid"]
                    }),
                ),
                make_tool(
                    "search_processes",
                    "Search processes by name",
                    json!({
                        "type": "object",
                        "properties": {
                            "query": { "type": "string", "description": "Search query" }
                        },
                        "required": ["query"]
                    }),
                ),
                make_tool(
                    "kill_process",
                    "Terminate a process by PID",
                    json!({
                        "type": "object",
                        "properties": {
                            "pid": { "type": "integer", "description": "Process ID to kill" },
                            "tree": { "type": "boolean", "description": "Kill entire process tree" }
                        },
                        "required": ["pid"]
                    }),
                ),
                make_tool(
                    "set_priority",
                    "Set process priority (realtime, high, above_normal, normal, below_normal, idle)",
                    json!({
                        "type": "object",
                        "properties": {
                            "pid": { "type": "integer", "description": "Process ID" },
                            "priority": { "type": "string", "description": "Priority level" }
                        },
                        "required": ["pid", "priority"]
                    }),
                ),
                make_tool(
                    "system_stats",
                    "Get system statistics (CPU, memory, hostname)",
                    json!({ "type": "object", "properties": {} }),
                ),
                make_tool(
                    "memory_summary",
                    "Get memory usage summary",
                    json!({ "type": "object", "properties": {} }),
                ),
            ];

            Ok(ListToolsResult {
                tools,
                next_cursor: None,
                meta: None,
            })
        }
    }

    fn call_tool(
        &self,
        request: CallToolRequest,
        _context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<CallToolResult, rmcp::ErrorData>> + Send + '_ {
        async move {
            let args = request.params.arguments.unwrap_or_default();
            let tool_name: &str = &request.params.name;

            let result = match tool_name {
                "list_processes" => {
                    let (mut processes, _) = self.refresh_cache().await;

                    if let Some(name) = args.get("name").and_then(|v| v.as_str()) {
                        let name_lower = name.to_lowercase();
                        processes.retain(|p| p.name.to_lowercase().contains(&name_lower));
                    }
                    if let Some(min_mem) = args.get("min_memory_mb").and_then(|v| v.as_f64()) {
                        processes.retain(|p| p.memory_mb >= min_mem as f32);
                    }
                    if let Some(limit) = args.get("limit").and_then(|v| v.as_u64()) {
                        processes.truncate(limit as usize);
                    }

                    serde_json::to_string_pretty(&processes).unwrap_or_default()
                }

                "top_cpu" => {
                    let (mut processes, _) = self.refresh_cache().await;
                    let limit = args.get("limit").and_then(|v| v.as_u64()).unwrap_or(10) as usize;

                    processes.sort_by(|a, b| {
                        b.cpu_percent
                            .partial_cmp(&a.cpu_percent)
                            .unwrap_or(std::cmp::Ordering::Equal)
                    });
                    processes.truncate(limit);

                    serde_json::to_string_pretty(&processes).unwrap_or_default()
                }

                "top_memory" => {
                    let (mut processes, _) = self.refresh_cache().await;
                    let limit = args.get("limit").and_then(|v| v.as_u64()).unwrap_or(10) as usize;

                    processes.sort_by(|a, b| {
                        b.memory_mb
                            .partial_cmp(&a.memory_mb)
                            .unwrap_or(std::cmp::Ordering::Equal)
                    });
                    processes.truncate(limit);

                    serde_json::to_string_pretty(&processes).unwrap_or_default()
                }

                "get_process" => {
                    let pid = args.get("pid").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
                    let (processes, _) = self.refresh_cache().await;

                    match processes.iter().find(|p| p.pid == pid) {
                        Some(p) => serde_json::to_string_pretty(&p).unwrap_or_default(),
                        None => format!("Process with PID {} not found", pid),
                    }
                }

                "search_processes" => {
                    let query = args.get("query").and_then(|v| v.as_str()).unwrap_or("");
                    let (processes, _) = self.refresh_cache().await;

                    let query_lower = query.to_lowercase();
                    let matches: Vec<_> = processes
                        .into_iter()
                        .filter(|p| p.name.to_lowercase().contains(&query_lower))
                        .collect();

                    serde_json::to_string_pretty(&matches).unwrap_or_default()
                }

                "kill_process" => {
                    let pid = args.get("pid").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
                    let tree = args.get("tree").and_then(|v| v.as_bool()).unwrap_or(false);

                    match crate::kill_process_impl(pid, tree).await {
                        Ok(_) => format!("Process {} terminated successfully", pid),
                        Err(e) => format!("Failed to terminate process {}: {}", pid, e),
                    }
                }

                "set_priority" => {
                    let pid = args.get("pid").and_then(|v| v.as_u64()).unwrap_or(0) as u32;
                    let priority = args
                        .get("priority")
                        .and_then(|v| v.as_str())
                        .unwrap_or("normal");

                    match crate::set_priority_impl(pid, priority).await {
                        Ok(_) => format!("Process {} priority set to {}", pid, priority),
                        Err(e) => format!("Failed to set priority: {}", e),
                    }
                }

                "system_stats" => {
                    let (_, stats) = self.refresh_cache().await;
                    serde_json::to_string_pretty(&stats).unwrap_or_default()
                }

                "memory_summary" => {
                    let (_, stats) = self.refresh_cache().await;
                    format!(
                        "Memory Usage:\n  Total: {} MB\n  Used: {} MB ({:.1}%)\n  Available: {} MB",
                        stats.memory.total_mb,
                        stats.memory.used_mb,
                        stats.memory.percent,
                        stats.memory.available_mb
                    )
                }

                _ => format!("Unknown tool: {}", tool_name),
            };

            Ok(CallToolResult::success(vec![Content::text(result)]))
        }
    }
}

/// Start the MCP server on stdio (for use as a child process)
pub async fn run_mcp_server() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    use rmcp::transport::stdio;

    let server = TaskManagerServer::new();
    let service = server.serve(stdio()).await?;
    service.waiting().await?;

    Ok(())
}
