using System.Diagnostics;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Text;
using System.Text.RegularExpressions;

namespace WslMonitor;

public class WslProcess
{
    public int Pid { get; set; }
    public string User { get; set; } = "";
    public double MemoryMB { get; set; }
    public double CpuPercent { get; set; }
    public string Command { get; set; } = "";
    public string ShortName => Path.GetFileName(Command.Split(' ')[0]);
}

public class MemoryInfo
{
    public long TotalMB { get; set; }
    public long UsedMB { get; set; }
    public long AvailableMB { get; set; }
    public int Percent => TotalMB > 0 ? (int)(UsedMB * 100 / TotalMB) : 0;
}

// Modern dark theme renderer
public class ModernMenuRenderer : ToolStripProfessionalRenderer
{
    private static readonly Color BackColor = Color.FromArgb(32, 32, 32);
    private static readonly Color HoverColor = Color.FromArgb(55, 55, 55);
    private static readonly Color BorderColor = Color.FromArgb(60, 60, 60);
    private static readonly Color TextColor = Color.FromArgb(240, 240, 240);
    private static readonly Color DimTextColor = Color.FromArgb(160, 160, 160);
    private static readonly Color AccentColor = Color.FromArgb(0, 120, 212);

    protected override void OnRenderToolStripBackground(ToolStripRenderEventArgs e)
    {
        using var brush = new SolidBrush(BackColor);
        e.Graphics.FillRectangle(brush, e.AffectedBounds);
    }

    protected override void OnRenderToolStripBorder(ToolStripRenderEventArgs e)
    {
        using var pen = new Pen(BorderColor);
        var rect = new Rectangle(0, 0, e.ToolStrip.Width - 1, e.ToolStrip.Height - 1);
        e.Graphics.DrawRectangle(pen, rect);
    }

    protected override void OnRenderMenuItemBackground(ToolStripItemRenderEventArgs e)
    {
        var rect = new Rectangle(4, 2, e.Item.Width - 8, e.Item.Height - 4);

        if (e.Item.Selected && e.Item.Enabled)
        {
            using var brush = new SolidBrush(HoverColor);
            using var path = CreateRoundedRectangle(rect, 4);
            e.Graphics.SmoothingMode = SmoothingMode.AntiAlias;
            e.Graphics.FillPath(brush, path);
        }
    }

    protected override void OnRenderItemText(ToolStripItemTextRenderEventArgs e)
    {
        e.TextColor = e.Item.Enabled ? TextColor : DimTextColor;
        base.OnRenderItemText(e);
    }

    protected override void OnRenderSeparator(ToolStripSeparatorRenderEventArgs e)
    {
        using var pen = new Pen(BorderColor);
        int y = e.Item.Height / 2;
        e.Graphics.DrawLine(pen, 8, y, e.Item.Width - 8, y);
    }

    protected override void OnRenderArrow(ToolStripArrowRenderEventArgs e)
    {
        e.ArrowColor = TextColor;
        base.OnRenderArrow(e);
    }

    private static GraphicsPath CreateRoundedRectangle(Rectangle rect, int radius)
    {
        var path = new GraphicsPath();
        path.AddArc(rect.X, rect.Y, radius * 2, radius * 2, 180, 90);
        path.AddArc(rect.Right - radius * 2, rect.Y, radius * 2, radius * 2, 270, 90);
        path.AddArc(rect.Right - radius * 2, rect.Bottom - radius * 2, radius * 2, radius * 2, 0, 90);
        path.AddArc(rect.X, rect.Bottom - radius * 2, radius * 2, radius * 2, 90, 90);
        path.CloseFigure();
        return path;
    }
}

public class WslMonitorApp : ApplicationContext
{
    private readonly NotifyIcon _trayIcon;
    private readonly System.Windows.Forms.Timer _timer;
    private readonly ContextMenuStrip _menu;
    private readonly ToolStripMenuItem _memoryItem;
    private readonly ToolStripMenuItem _availableItem;
    private readonly List<ToolStripMenuItem> _processItems = new();
    private List<WslProcess> _lastProcesses = new();

    private const int WarnThreshold = 70;
    private const int CritThreshold = 85;
    private const int PollIntervalMs = 2000;

    private int _lastAlertLevel = 0;
    private MemoryInfo _lastMemory = new();

    public WslMonitorApp()
    {
        _menu = new ContextMenuStrip();
        _menu.Renderer = new ModernMenuRenderer();
        _menu.BackColor = Color.FromArgb(32, 32, 32);
        _menu.ForeColor = Color.FromArgb(240, 240, 240);
        _menu.Font = new Font("Segoe UI", 9.5f);
        _menu.Padding = new Padding(4);
        _menu.ShowImageMargin = false;

        _memoryItem = new ToolStripMenuItem("Loading...") { Enabled = false };
        _menu.Items.Add(_memoryItem);

        _availableItem = new ToolStripMenuItem("") { Enabled = false };
        _menu.Items.Add(_availableItem);

        _menu.Items.Add(new ToolStripSeparator());

        // Process items
        for (int i = 0; i < 8; i++)
        {
            var item = new ToolStripMenuItem("") { Visible = false };
            item.Click += ProcessItem_Click;
            _processItems.Add(item);
            _menu.Items.Add(item);
        }

        _menu.Items.Add(new ToolStripSeparator());

        // Quick kill submenu
        var killMenu = new ToolStripMenuItem("Quick Kill");
        killMenu.DropDown.BackColor = Color.FromArgb(32, 32, 32);
        killMenu.DropDownItems.Add(CreateKillItem("All Node", "node"));
        killMenu.DropDownItems.Add(CreateKillItem("All PHP", "php"));
        killMenu.DropDownItems.Add(new ToolStripSeparator());
        killMenu.DropDownItems.Add(CreateKillItem("Intelephense", "intelephense"));
        killMenu.DropDownItems.Add(CreateKillItem("PHPStan", "phpstan"));
        killMenu.DropDownItems.Add(CreateKillItem("Psalm", "psalm"));
        killMenu.DropDownItems.Add(CreateKillItem("TypeScript", "tsserver"));
        killMenu.DropDownItems.Add(CreateKillItem("Composer", "composer"));
        _menu.Items.Add(killMenu);

        var refreshItem = new ToolStripMenuItem("Refresh");
        refreshItem.Click += (s, e) => UpdateStatus();
        _menu.Items.Add(refreshItem);

        _menu.Items.Add(new ToolStripSeparator());

        var exitItem = new ToolStripMenuItem("Exit");
        exitItem.Click += (s, e) => ExitApp();
        _menu.Items.Add(exitItem);

        _trayIcon = new NotifyIcon
        {
            Icon = CreateIcon(Color.Gray),
            Text = "WSL Monitor",
            Visible = true,
            ContextMenuStrip = _menu
        };

        _trayIcon.MouseClick += TrayIcon_MouseClick;

        _timer = new System.Windows.Forms.Timer { Interval = PollIntervalMs };
        _timer.Tick += (s, e) => UpdateStatus();
        _timer.Start();

        UpdateStatus();
    }

    private ToolStripMenuItem CreateKillItem(string label, string pattern)
    {
        var item = new ToolStripMenuItem(label);
        item.Click += (s, e) =>
        {
            KillByPattern(pattern);
            UpdateStatus();
        };
        return item;
    }

    private void TrayIcon_MouseClick(object? sender, MouseEventArgs e)
    {
        if (e.Button == MouseButtons.Left)
        {
            var sb = new StringBuilder();
            sb.AppendLine($"Memory: {_lastMemory.UsedMB:N0} / {_lastMemory.TotalMB:N0} MB");
            sb.AppendLine();
            foreach (var proc in _lastProcesses.Take(5))
            {
                sb.AppendLine($"  {proc.MemoryMB:F0} MB  {proc.ShortName}");
            }

            _trayIcon.BalloonTipTitle = $"WSL  {_lastMemory.Percent}%";
            _trayIcon.BalloonTipText = sb.ToString();
            _trayIcon.BalloonTipIcon = _lastMemory.Percent >= CritThreshold ? ToolTipIcon.Error
                                     : _lastMemory.Percent >= WarnThreshold ? ToolTipIcon.Warning
                                     : ToolTipIcon.Info;
            _trayIcon.ShowBalloonTip(5000);
        }
    }

    private void ProcessItem_Click(object? sender, EventArgs e)
    {
        if (sender is ToolStripMenuItem item && item.Tag is WslProcess proc)
        {
            var result = MessageBox.Show(
                $"Kill process?\n\nPID: {proc.Pid}\nCommand: {proc.Command}\nMemory: {proc.MemoryMB:F0} MB",
                "Confirm",
                MessageBoxButtons.YesNo,
                MessageBoxIcon.Warning);

            if (result == DialogResult.Yes)
            {
                KillProcess(proc.Pid);
                UpdateStatus();
            }
        }
    }

    private void UpdateStatus()
    {
        try
        {
            var memory = GetMemoryInfo();
            var processes = GetTopProcesses(8);

            _lastMemory = memory;
            _lastProcesses = processes;

            _memoryItem.Text = $"Memory   {memory.UsedMB:N0} MB   {memory.Percent}%";
            _availableItem.Text = $"Available   {memory.AvailableMB:N0} MB";

            Color iconColor;
            if (memory.Percent >= CritThreshold)
                iconColor = Color.FromArgb(232, 17, 35);
            else if (memory.Percent >= WarnThreshold)
                iconColor = Color.FromArgb(255, 140, 0);
            else
                iconColor = Color.FromArgb(16, 185, 129);

            _trayIcon.Icon?.Dispose();
            _trayIcon.Icon = CreateIcon(iconColor);

            var top3 = string.Join("  ", processes.Take(3).Select(p => $"{p.ShortName}:{p.MemoryMB:F0}M"));
            _trayIcon.Text = $"WSL {memory.Percent}%  {memory.UsedMB:N0} MB\n{top3}".Truncate(127);

            CheckAlerts(memory, processes);

            for (int i = 0; i < _processItems.Count; i++)
            {
                if (i < processes.Count)
                {
                    var proc = processes[i];
                    _processItems[i].Text = $"{proc.MemoryMB,5:F0} MB   {proc.ShortName.Truncate(25)}";
                    _processItems[i].Tag = proc;
                    _processItems[i].Visible = true;
                }
                else
                {
                    _processItems[i].Visible = false;
                }
            }
        }
        catch
        {
            _memoryItem.Text = "WSL not responding";
            _availableItem.Text = "";
            _trayIcon.Icon = CreateIcon(Color.Gray);
            _trayIcon.Text = "WSL Monitor - Disconnected";
        }
    }

    private void CheckAlerts(MemoryInfo memory, List<WslProcess> processes)
    {
        int alertLevel = memory.Percent >= CritThreshold ? 2
                       : memory.Percent >= WarnThreshold ? 1
                       : 0;

        if (alertLevel > _lastAlertLevel)
        {
            var sb = new StringBuilder();
            sb.AppendLine($"{memory.UsedMB:N0} / {memory.TotalMB:N0} MB");
            sb.AppendLine();
            foreach (var proc in processes.Take(3))
            {
                sb.AppendLine($"  {proc.MemoryMB:F0} MB  {proc.ShortName}");
            }

            _trayIcon.BalloonTipTitle = alertLevel == 2 ? "WSL CRITICAL" : "WSL Warning";
            _trayIcon.BalloonTipText = sb.ToString();
            _trayIcon.BalloonTipIcon = alertLevel == 2 ? ToolTipIcon.Error : ToolTipIcon.Warning;
            _trayIcon.ShowBalloonTip(8000);
        }

        _lastAlertLevel = alertLevel;
    }

    private static Icon CreateIcon(Color color)
    {
        using var bmp = new Bitmap(16, 16);
        using var g = Graphics.FromImage(bmp);
        g.SmoothingMode = SmoothingMode.AntiAlias;

        using var brush = new SolidBrush(color);
        g.FillEllipse(brush, 1, 1, 13, 13);

        using var pen = new Pen(Color.FromArgb(40, 0, 0, 0), 1);
        g.DrawEllipse(pen, 1, 1, 13, 13);

        return Icon.FromHandle(bmp.GetHicon());
    }

    private static MemoryInfo GetMemoryInfo()
    {
        var output = RunWslCommand("free -m | grep Mem:");
        var match = Regex.Match(output, @"Mem:\s+(\d+)\s+(\d+)\s+\d+\s+\d+\s+\d+\s+(\d+)");

        if (match.Success)
        {
            return new MemoryInfo
            {
                TotalMB = long.Parse(match.Groups[1].Value),
                UsedMB = long.Parse(match.Groups[2].Value),
                AvailableMB = long.Parse(match.Groups[3].Value)
            };
        }

        return new MemoryInfo();
    }

    private static List<WslProcess> GetTopProcesses(int count)
    {
        var output = RunWslCommand($"ps aux --sort=-%mem | head -{count + 1}");
        var lines = output.Split('\n', StringSplitOptions.RemoveEmptyEntries).Skip(1);
        var processes = new List<WslProcess>();

        foreach (var line in lines)
        {
            var parts = Regex.Split(line.Trim(), @"\s+");
            if (parts.Length >= 11)
            {
                processes.Add(new WslProcess
                {
                    User = parts[0],
                    Pid = int.TryParse(parts[1], out var pid) ? pid : 0,
                    CpuPercent = double.TryParse(parts[2], out var cpu) ? cpu : 0,
                    MemoryMB = double.TryParse(parts[5], out var rss) ? rss / 1024 : 0,
                    Command = string.Join(" ", parts.Skip(10))
                });
            }
        }

        return processes;
    }

    private static void KillProcess(int pid) => RunWslCommand($"kill -9 {pid}");
    private static void KillByPattern(string pattern) => RunWslCommand($"pkill -9 -f {pattern}");

    private static string RunWslCommand(string command)
    {
        var psi = new ProcessStartInfo
        {
            FileName = "wsl",
            Arguments = $"-e bash -c \"{command.Replace("\"", "\\\"")}\"",
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        using var process = Process.Start(psi);
        if (process == null) return "";

        var output = process.StandardOutput.ReadToEnd();
        process.WaitForExit(2000);
        return output;
    }

    private void ExitApp()
    {
        _timer.Stop();
        _trayIcon.Visible = false;
        _trayIcon.Icon?.Dispose();
        Application.Exit();
    }
}

public static class StringExtensions
{
    public static string Truncate(this string value, int maxLength)
    {
        if (string.IsNullOrEmpty(value)) return value;
        return value.Length <= maxLength ? value : value[..(maxLength - 3)] + "...";
    }
}

static class Program
{
    [STAThread]
    static void Main()
    {
        Application.EnableVisualStyles();
        Application.SetCompatibleTextRenderingDefault(false);

        using var mutex = new Mutex(true, "WslMonitor_SingleInstance", out bool isNew);
        if (!isNew)
        {
            MessageBox.Show("WSL Monitor is already running.", "WSL Monitor",
                MessageBoxButtons.OK, MessageBoxIcon.Information);
            return;
        }

        Application.Run(new WslMonitorApp());
    }
}
