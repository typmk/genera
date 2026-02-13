#![cfg_attr(not(debug_assertions), windows_subsystem = "windows")]

use crossbeam_channel::{bounded, Receiver, Sender};
use eframe::egui::{self, Color32, RichText, Vec2, ViewportCommand};
use egui_plot::{Line, Plot, PlotPoints};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::VecDeque;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::{Duration, Instant};
use tray_icon::menu::{Menu, MenuEvent, MenuItem, PredefinedMenuItem};
use tray_icon::{Icon, TrayIcon, TrayIconBuilder};

#[cfg(windows)]
use std::os::windows::process::CommandExt;

#[cfg(windows)]
const CREATE_NO_WINDOW: u32 = 0x08000000;

// ============================================================================
// Data Types (immutable where possible)
// ============================================================================

#[derive(Clone, Debug, Default, Serialize, Deserialize, PartialEq)]
struct WslProcess {
    pid: u32,
    ppid: u32,
    user: String,
    cpu_percent: f32,
    memory_mb: f32,
    memory_percent: f32,
    command: String,
    short_name: String,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct MemoryInfo {
    total_mb: u64,
    used_mb: u64,
    available_mb: u64,
    percent: f32,
}

impl MemoryInfo {
    fn status_color(&self) -> Color32 {
        match self.percent {
            p if p > 85.0 => Color32::from_rgb(232, 17, 35),  // Red
            p if p > 70.0 => Color32::from_rgb(255, 140, 0),  // Orange
            _ => Color32::from_rgb(16, 185, 129),             // Green
        }
    }
}

#[derive(Clone, Debug, Default)]
struct SystemStats {
    memory: MemoryInfo,
    cpu_percent: f32,
    processes: Vec<WslProcess>,
    load_avg: [f32; 3],
}

#[derive(Clone, Debug)]
enum WslCommand {
    Kill(u32),
    KillTree(u32),
    KillPattern(String),
    Refresh,
}

#[derive(Clone, Debug)]
enum TrayAction {
    Show,
    Hide,
    Quit,
    Refresh,
    KillNode,
    KillPhp,
}

// ============================================================================
// WSL Command Execution (pure functions)
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
        .map(|o| String::from_utf8_lossy(&o.stdout).into_owned())
}

// ============================================================================
// Parsing Functions (pure, no side effects)
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
    let percent = (total > 0).then(|| used as f32 / total as f32 * 100.0).unwrap_or(0.0);

    MemoryInfo { total_mb: total, used_mb: used, available_mb: available, percent }
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
    let parts: Vec<&str> = line.split_whitespace().collect();
    (parts.len() >= 11).then(|| {
        let command = parts[10..].join(" ");
        let short_name = command
            .split_whitespace()
            .next()
            .and_then(|s| s.split('/').last())
            .unwrap_or("")
            .to_string();

        WslProcess {
            user: parts[0].to_string(),
            pid: parts[1].parse().unwrap_or(0),
            ppid: 0,
            cpu_percent: parts[2].parse().unwrap_or(0.0),
            memory_percent: parts[3].parse().unwrap_or(0.0),
            memory_mb: parts[5].parse::<f32>().unwrap_or(0.0) / 1024.0,
            command,
            short_name,
        }
    })
}

fn parse_ps_output(s: &str) -> Vec<WslProcess> {
    s.lines()
        .skip(1)
        .filter_map(parse_ps_line)
        .collect()
}

// ============================================================================
// Data Collection (combines pure functions)
// ============================================================================

fn collect_wsl_stats() -> Option<SystemStats> {
    let memory = run_wsl(&["-e", "cat", "/proc/meminfo"]).map(|s| parse_meminfo(&s))?;
    let load_avg = run_wsl(&["-e", "cat", "/proc/loadavg"]).map(|s| parse_loadavg(&s))?;
    let processes = run_wsl(&["-e", "bash", "-c", "ps aux --sort=-%mem | head -50"])
        .map(|s| parse_ps_output(&s))?;

    let cpu_percent = processes.iter().map(|p| p.cpu_percent).sum::<f32>().min(100.0);

    Some(SystemStats { memory, cpu_percent, processes, load_avg })
}

fn execute_wsl_command(cmd: &WslCommand) {
    match cmd {
        WslCommand::Kill(pid) => { run_wsl(&["-e", "kill", "-9", &pid.to_string()]); }
        WslCommand::KillTree(pid) => {
            let script = format!("pkill -9 -P {pid}; kill -9 {pid}");
            run_wsl(&["-e", "bash", "-c", &script]);
        }
        WslCommand::KillPattern(pattern) => { run_wsl(&["-e", "pkill", "-9", "-f", pattern]); }
        WslCommand::Refresh => {}
    }
}

// ============================================================================
// History (functional ring buffer)
// ============================================================================

#[derive(Clone)]
struct History {
    cpu: VecDeque<f32>,
    memory: VecDeque<f32>,
    max_points: usize,
}

impl History {
    fn new(max_points: usize) -> Self {
        Self {
            cpu: VecDeque::with_capacity(max_points),
            memory: VecDeque::with_capacity(max_points),
            max_points,
        }
    }

    fn push(&mut self, cpu: f32, memory: f32) {
        if self.cpu.len() >= self.max_points {
            self.cpu.pop_front();
            self.memory.pop_front();
        }
        self.cpu.push_back(cpu);
        self.memory.push_back(memory);
    }

    fn to_plot_points(data: &VecDeque<f32>) -> PlotPoints {
        PlotPoints::from_iter(
            data.iter()
                .enumerate()
                .map(|(i, &v)| [i as f64, v as f64])
        )
    }
}

// ============================================================================
// Tray Icon
// ============================================================================

fn create_tray_icon(percent: f32) -> Icon {
    let size = 32u32;
    let mut rgba = vec![0u8; (size * size * 4) as usize];

    let color = match percent {
        p if p > 85.0 => [232u8, 17, 35, 255],   // Red
        p if p > 70.0 => [255u8, 140, 0, 255],   // Orange
        _ => [16u8, 185, 129, 255],              // Green
    };

    let center = size as f32 / 2.0;
    let radius = (size as f32 / 2.0) - 2.0;

    for y in 0..size {
        for x in 0..size {
            let dx = x as f32 - center;
            let dy = y as f32 - center;
            let dist = (dx * dx + dy * dy).sqrt();

            if dist <= radius {
                let idx = ((y * size + x) * 4) as usize;
                rgba[idx..idx + 4].copy_from_slice(&color);
            }
        }
    }

    Icon::from_rgba(rgba, size, size).expect("Failed to create icon")
}

fn create_tray_menu() -> (Menu, MenuItem, MenuItem, MenuItem, MenuItem, MenuItem) {
    let menu = Menu::new();

    let show_item = MenuItem::new("Show Window", true, None);
    let refresh_item = MenuItem::new("Refresh", true, None);
    let kill_node = MenuItem::new("Kill All Node", true, None);
    let kill_php = MenuItem::new("Kill All PHP", true, None);
    let quit_item = MenuItem::new("Exit", true, None);

    menu.append(&show_item).unwrap();
    menu.append(&PredefinedMenuItem::separator()).unwrap();
    menu.append(&refresh_item).unwrap();
    menu.append(&kill_node).unwrap();
    menu.append(&kill_php).unwrap();
    menu.append(&PredefinedMenuItem::separator()).unwrap();
    menu.append(&quit_item).unwrap();

    (menu, show_item, refresh_item, kill_node, kill_php, quit_item)
}

// ============================================================================
// Application State
// ============================================================================

struct WslTaskManager {
    stats: Arc<RwLock<SystemStats>>,
    history: History,
    cmd_tx: Sender<WslCommand>,
    tray_rx: Receiver<TrayAction>,
    search_query: String,
    selected_pid: Option<u32>,
    sort_by: SortColumn,
    sort_ascending: bool,
    tab: Tab,
    last_update: Instant,
    minimized_to_tray: bool,
    should_quit: Arc<AtomicBool>,
    tray_icon: Arc<parking_lot::Mutex<TrayIcon>>,
    // Custom maximize state for multi-monitor support
    is_maximized: bool,
    restore_rect: Option<egui::Rect>,
}

#[derive(Clone, Copy, PartialEq)]
enum Tab { Processes, Performance }

#[derive(Clone, Copy, PartialEq)]
enum SortColumn { Name, Pid, Cpu, Memory }

impl WslTaskManager {
    fn new(
        cc: &eframe::CreationContext<'_>,
        tray_rx: Receiver<TrayAction>,
        should_quit: Arc<AtomicBool>,
        tray_icon: Arc<parking_lot::Mutex<TrayIcon>>,
    ) -> Self {
        let stats = Arc::new(RwLock::new(SystemStats::default()));
        let stats_clone = Arc::clone(&stats);
        let (cmd_tx, cmd_rx): (Sender<WslCommand>, Receiver<WslCommand>) = bounded(16);

        // Background data collection
        thread::spawn(move || {
            loop {
                while let Ok(cmd) = cmd_rx.try_recv() {
                    execute_wsl_command(&cmd);
                }
                if let Some(new_stats) = collect_wsl_stats() {
                    *stats_clone.write() = new_stats;
                }
                thread::sleep(Duration::from_millis(1500));
            }
        });

        // Dark theme - let system handle DPI scaling
        cc.egui_ctx.set_visuals(egui::Visuals::dark());

        Self {
            stats,
            history: History::new(60),
            cmd_tx,
            tray_rx,
            search_query: String::new(),
            selected_pid: None,
            sort_by: SortColumn::Memory,
            sort_ascending: false,
            tab: Tab::Processes,
            last_update: Instant::now(),
            minimized_to_tray: false,
            should_quit,
            tray_icon,
            is_maximized: false,
            restore_rect: None,
        }
    }

    fn toggle_maximize(&mut self, ctx: &egui::Context) {
        if self.is_maximized {
            // Restore to previous size
            if let Some(rect) = self.restore_rect {
                ctx.send_viewport_cmd(ViewportCommand::InnerSize(rect.size()));
                ctx.send_viewport_cmd(ViewportCommand::OuterPosition(rect.min));
            }
            self.is_maximized = false;
        } else {
            // Save current rect and maximize to current monitor
            ctx.input(|i| {
                if let Some(outer_rect) = i.viewport().outer_rect {
                    self.restore_rect = Some(outer_rect);
                }
                if let Some(monitor_size) = i.viewport().monitor_size {
                    // Get fullscreen on current monitor
                    ctx.send_viewport_cmd(ViewportCommand::Fullscreen(false));
                    ctx.send_viewport_cmd(ViewportCommand::InnerSize(monitor_size));
                    // Position at origin of current monitor (approximation)
                    if let Some(outer_rect) = i.viewport().outer_rect {
                        // Estimate monitor origin from current position
                        let monitor_x = (outer_rect.center().x / monitor_size.x).floor() * monitor_size.x;
                        let monitor_y = (outer_rect.center().y / monitor_size.y).floor() * monitor_size.y;
                        ctx.send_viewport_cmd(ViewportCommand::OuterPosition(
                            egui::pos2(monitor_x, monitor_y)
                        ));
                    }
                }
            });
            self.is_maximized = true;
        }
    }

    fn sorted_processes(&self, processes: &[WslProcess]) -> Vec<WslProcess> {
        let query = self.search_query.to_lowercase();

        let mut filtered: Vec<_> = processes
            .iter()
            .filter(|p| {
                query.is_empty()
                    || p.short_name.to_lowercase().contains(&query)
                    || p.command.to_lowercase().contains(&query)
                    || p.pid.to_string().contains(&query)
            })
            .cloned()
            .collect();

        filtered.sort_by(|a, b| {
            let cmp = match self.sort_by {
                SortColumn::Name => a.short_name.to_lowercase().cmp(&b.short_name.to_lowercase()),
                SortColumn::Pid => a.pid.cmp(&b.pid),
                SortColumn::Cpu => a.cpu_percent.partial_cmp(&b.cpu_percent).unwrap_or(std::cmp::Ordering::Equal),
                SortColumn::Memory => a.memory_mb.partial_cmp(&b.memory_mb).unwrap_or(std::cmp::Ordering::Equal),
            };
            if self.sort_ascending { cmp } else { cmp.reverse() }
        });

        filtered
    }

    fn toggle_sort(&mut self, col: SortColumn) {
        if self.sort_by == col {
            self.sort_ascending = !self.sort_ascending;
        } else {
            self.sort_by = col;
            self.sort_ascending = false;
        }
    }

    fn update_tray_icon(&self, percent: f32) {
        let tray = self.tray_icon.lock();
        let _ = tray.set_icon(Some(create_tray_icon(percent)));
        let _ = tray.set_tooltip(Some(format!("WSL Memory: {:.0}%", percent)));
    }
}

impl eframe::App for WslTaskManager {
    fn update(&mut self, ctx: &egui::Context, _frame: &mut eframe::Frame) {
        // Handle tray menu events
        while let Ok(action) = self.tray_rx.try_recv() {
            match action {
                TrayAction::Show => {
                    self.minimized_to_tray = false;
                    ctx.send_viewport_cmd(ViewportCommand::Visible(true));
                    ctx.send_viewport_cmd(ViewportCommand::Focus);
                }
                TrayAction::Hide => {
                    self.minimized_to_tray = true;
                    ctx.send_viewport_cmd(ViewportCommand::Visible(false));
                }
                TrayAction::Quit => {
                    self.should_quit.store(true, Ordering::SeqCst);
                    ctx.send_viewport_cmd(ViewportCommand::Close);
                }
                TrayAction::Refresh => {
                    let _ = self.cmd_tx.send(WslCommand::Refresh);
                }
                TrayAction::KillNode => {
                    let _ = self.cmd_tx.send(WslCommand::KillPattern("node".into()));
                }
                TrayAction::KillPhp => {
                    let _ = self.cmd_tx.send(WslCommand::KillPattern("php".into()));
                }
            }
        }

        let stats = self.stats.read().clone();

        // Update history and tray icon
        if self.last_update.elapsed() >= Duration::from_secs(1) {
            self.history.push(stats.cpu_percent, stats.memory.percent);
            self.update_tray_icon(stats.memory.percent);
            self.last_update = Instant::now();
        }

        // Top panel - stats overview
        egui::TopBottomPanel::top("top_panel").show(ctx, |ui| {
            ui.add_space(8.0);
            ui.horizontal(|ui| {
                ui.colored_label(
                    stats.memory.status_color(),
                    RichText::new(format!("MEM: {:.0}%", stats.memory.percent)).strong().size(14.0)
                );
                ui.label(RichText::new(format!("{} / {} MB", stats.memory.used_mb, stats.memory.total_mb)).size(12.0));

                ui.separator();
                ui.label(RichText::new(format!("CPU: {:.1}%", stats.cpu_percent)).strong().size(14.0));

                ui.separator();
                ui.label(RichText::new(format!("Load: {:.2} {:.2} {:.2}",
                    stats.load_avg[0], stats.load_avg[1], stats.load_avg[2])).size(12.0));

                ui.separator();
                ui.label(RichText::new(format!("{} processes", stats.processes.len())).size(12.0));

                ui.with_layout(egui::Layout::right_to_left(egui::Align::Center), |ui| {
                    if ui.button("⟳").on_hover_text("Refresh").clicked() {
                        let _ = self.cmd_tx.send(WslCommand::Refresh);
                    }
                    let max_icon = if self.is_maximized { "❐" } else { "□" };
                    let max_tip = if self.is_maximized { "Restore" } else { "Maximize" };
                    if ui.button(max_icon).on_hover_text(max_tip).clicked() {
                        self.toggle_maximize(ctx);
                    }
                    if ui.button("▽").on_hover_text("Minimize to tray").clicked() {
                        self.minimized_to_tray = true;
                        ctx.send_viewport_cmd(ViewportCommand::Visible(false));
                    }
                });
            });
            ui.add_space(8.0);
        });

        // Tab bar
        egui::TopBottomPanel::top("tabs").show(ctx, |ui| {
            ui.horizontal(|ui| {
                ui.selectable_value(&mut self.tab, Tab::Processes, RichText::new("Processes").size(13.0));
                ui.selectable_value(&mut self.tab, Tab::Performance, RichText::new("Performance").size(13.0));
            });
        });

        // Main content
        egui::CentralPanel::default().show(ctx, |ui| {
            match self.tab {
                Tab::Processes => self.show_processes(ui, &stats),
                Tab::Performance => self.show_performance(ui, &stats),
            }
        });

        // Bottom panel - actions
        if self.selected_pid.is_some() {
            egui::TopBottomPanel::bottom("actions").show(ctx, |ui| {
                ui.add_space(6.0);
                ui.horizontal(|ui| {
                    if let Some(pid) = self.selected_pid {
                        if ui.button(RichText::new("Kill Process").size(12.0)).clicked() {
                            let _ = self.cmd_tx.send(WslCommand::Kill(pid));
                            self.selected_pid = None;
                        }
                        if ui.button(RichText::new("Kill Tree").size(12.0)).clicked() {
                            let _ = self.cmd_tx.send(WslCommand::KillTree(pid));
                            self.selected_pid = None;
                        }
                    }
                    ui.separator();
                    if ui.button(RichText::new("Kill All Node").size(12.0)).clicked() {
                        let _ = self.cmd_tx.send(WslCommand::KillPattern("node".into()));
                    }
                    if ui.button(RichText::new("Kill All PHP").size(12.0)).clicked() {
                        let _ = self.cmd_tx.send(WslCommand::KillPattern("php".into()));
                    }
                });
                ui.add_space(6.0);
            });
        }

        ctx.request_repaint_after(Duration::from_millis(500));
    }

}

impl WslTaskManager {
    fn show_processes(&mut self, ui: &mut egui::Ui, stats: &SystemStats) {
        ui.horizontal(|ui| {
            ui.label("🔍");
            ui.add(egui::TextEdit::singleline(&mut self.search_query)
                .hint_text("Search processes...")
                .desired_width(250.0));
            if !self.search_query.is_empty() && ui.button("✕").clicked() {
                self.search_query.clear();
            }
        });

        ui.add_space(6.0);

        let processes = self.sorted_processes(&stats.processes);

        egui::ScrollArea::vertical().auto_shrink([false, false]).show(ui, |ui| {
            egui::Grid::new("process_grid")
                .num_columns(4)
                .striped(true)
                .min_col_width(80.0)
                .spacing([12.0, 4.0])
                .show(ui, |ui| {
                    // Header
                    let header = |ui: &mut egui::Ui, label: &str, col: SortColumn, mgr: &mut WslTaskManager| {
                        let text = if mgr.sort_by == col {
                            format!("{} {}", label, if mgr.sort_ascending { "▲" } else { "▼" })
                        } else {
                            label.to_string()
                        };
                        if ui.selectable_label(mgr.sort_by == col, RichText::new(text).strong().size(12.0)).clicked() {
                            mgr.toggle_sort(col);
                        }
                    };

                    header(ui, "Name", SortColumn::Name, self);
                    header(ui, "PID", SortColumn::Pid, self);
                    header(ui, "CPU %", SortColumn::Cpu, self);
                    header(ui, "Memory", SortColumn::Memory, self);
                    ui.end_row();

                    for proc in &processes {
                        let selected = self.selected_pid == Some(proc.pid);

                        let resp = ui.selectable_label(selected, RichText::new(&proc.short_name).size(11.0));
                        if resp.clicked() {
                            self.selected_pid = if selected { None } else { Some(proc.pid) };
                        }
                        resp.on_hover_text(&proc.command);

                        ui.label(RichText::new(proc.pid.to_string()).size(11.0));

                        let cpu_color = match proc.cpu_percent {
                            c if c > 50.0 => Color32::from_rgb(232, 17, 35),
                            c if c > 20.0 => Color32::from_rgb(255, 140, 0),
                            _ => Color32::GRAY,
                        };
                        ui.colored_label(cpu_color, RichText::new(format!("{:.1}", proc.cpu_percent)).size(11.0));

                        let mem_color = match proc.memory_mb {
                            m if m > 500.0 => Color32::from_rgb(232, 17, 35),
                            m if m > 200.0 => Color32::from_rgb(255, 140, 0),
                            _ => Color32::GRAY,
                        };
                        ui.colored_label(mem_color, RichText::new(format!("{:.0} MB", proc.memory_mb)).size(11.0));

                        ui.end_row();
                    }
                });
        });
    }

    fn show_performance(&mut self, ui: &mut egui::Ui, stats: &SystemStats) {
        ui.columns(2, |cols| {
            cols[0].heading("CPU Usage");
            Plot::new("cpu_plot")
                .height(160.0)
                .include_y(0.0)
                .include_y(100.0)
                .show_axes([false, true])
                .allow_zoom(false)
                .allow_drag(false)
                .show(&mut cols[0], |plot_ui| {
                    plot_ui.line(
                        Line::new(History::to_plot_points(&self.history.cpu))
                            .color(Color32::from_rgb(59, 130, 246))
                            .fill(0.0)
                    );
                });
            cols[0].label(RichText::new(format!("Current: {:.1}%", stats.cpu_percent)).size(12.0));
            cols[0].label(RichText::new(format!("Load: {:.2}  {:.2}  {:.2}",
                stats.load_avg[0], stats.load_avg[1], stats.load_avg[2])).size(11.0));

            cols[1].heading("Memory Usage");
            Plot::new("mem_plot")
                .height(160.0)
                .include_y(0.0)
                .include_y(100.0)
                .show_axes([false, true])
                .allow_zoom(false)
                .allow_drag(false)
                .show(&mut cols[1], |plot_ui| {
                    plot_ui.line(
                        Line::new(History::to_plot_points(&self.history.memory))
                            .color(Color32::from_rgb(16, 185, 129))
                            .fill(0.0)
                    );
                });
            cols[1].label(RichText::new(format!("Used: {} / {} MB ({:.0}%)",
                stats.memory.used_mb, stats.memory.total_mb, stats.memory.percent)).size(12.0));
            cols[1].label(RichText::new(format!("Available: {} MB", stats.memory.available_mb)).size(11.0));
        });

        ui.add_space(16.0);
        ui.heading("Top Memory Consumers");

        egui::Grid::new("top_mem").striped(true).spacing([8.0, 4.0]).show(ui, |ui| {
            for proc in stats.processes.iter().take(5) {
                ui.label(RichText::new(&proc.short_name).size(11.0));

                let bar_width = (proc.memory_mb / 2000.0).min(1.0) * 200.0;
                let (rect, _) = ui.allocate_exact_size(Vec2::new(200.0, 14.0), egui::Sense::hover());
                ui.painter().rect_filled(
                    egui::Rect::from_min_size(rect.min, Vec2::new(bar_width, 14.0)),
                    3.0,
                    Color32::from_rgb(59, 130, 246)
                );

                ui.label(RichText::new(format!("{:.0} MB", proc.memory_mb)).size(11.0));
                ui.end_row();
            }
        });
    }
}

// ============================================================================
// Main with tray event loop
// ============================================================================

fn main() -> eframe::Result<()> {
    let should_quit = Arc::new(AtomicBool::new(false));
    let should_quit_clone = Arc::clone(&should_quit);

    let (tray_tx, tray_rx) = bounded::<TrayAction>(16);

    // Create tray menu items
    let (menu, show_item, refresh_item, kill_node, kill_php, quit_item) = create_tray_menu();

    // Build single tray icon with menu
    let tray_icon = TrayIconBuilder::new()
        .with_menu(Box::new(menu))
        .with_tooltip("WSL Task Manager")
        .with_icon(create_tray_icon(0.0))
        .build()
        .expect("Failed to create tray icon");

    let tray_icon = Arc::new(parking_lot::Mutex::new(tray_icon));
    let tray_icon_clone = Arc::clone(&tray_icon);

    // Handle menu events in separate thread
    let tray_tx_clone = tray_tx.clone();
    let show_id = show_item.id().clone();
    let refresh_id = refresh_item.id().clone();
    let kill_node_id = kill_node.id().clone();
    let kill_php_id = kill_php.id().clone();
    let quit_id = quit_item.id().clone();

    thread::spawn(move || {
        loop {
            if let Ok(event) = MenuEvent::receiver().recv() {
                let action = if event.id == show_id {
                    Some(TrayAction::Show)
                } else if event.id == refresh_id {
                    Some(TrayAction::Refresh)
                } else if event.id == kill_node_id {
                    Some(TrayAction::KillNode)
                } else if event.id == kill_php_id {
                    Some(TrayAction::KillPhp)
                } else if event.id == quit_id {
                    Some(TrayAction::Quit)
                } else {
                    None
                };

                if let Some(action) = action {
                    let _ = tray_tx_clone.send(action);
                }
            }
        }
    });

    let options = eframe::NativeOptions {
        viewport: egui::ViewportBuilder::default()
            .with_inner_size([900.0, 650.0])
            .with_min_inner_size([600.0, 400.0])
            .with_title("WSL Task Manager")
            .with_close_button(true)
            .with_minimize_button(true)
            .with_maximize_button(false) // We handle maximize ourselves for multi-monitor
            .with_resizable(true),
        centered: true,
        ..Default::default()
    };

    eframe::run_native(
        "WSL Task Manager",
        options,
        Box::new(move |cc| Ok(Box::new(WslTaskManager::new(cc, tray_rx, should_quit_clone, tray_icon_clone)))),
    )
}
