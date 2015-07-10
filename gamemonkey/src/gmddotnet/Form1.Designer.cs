namespace gmDebuggerNet
{
  partial class MainForm
  {
    /// <summary>
    /// Required designer variable.
    /// </summary>
    private System.ComponentModel.IContainer components = null;

    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
    protected override void Dispose(bool disposing)
    {
      if (disposing && (components != null))
      {
        components.Dispose();
      }
      base.Dispose(disposing);
    }

    #region Windows Form Designer generated code

    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    private void InitializeComponent()
    {
      System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
      this.m_menu = new System.Windows.Forms.MenuStrip();
      this.fileToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.exitToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.helpToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.aboutToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
      this.m_toolBar = new System.Windows.Forms.ToolStrip();
      this.buttonStepInto = new System.Windows.Forms.ToolStripButton();
      this.buttonStepOver = new System.Windows.Forms.ToolStripButton();
      this.buttonStepOut = new System.Windows.Forms.ToolStripButton();
      this.buttonRun = new System.Windows.Forms.ToolStripButton();
      this.buttonStopDebugging = new System.Windows.Forms.ToolStripButton();
      this.buttonToggleBreakpoint = new System.Windows.Forms.ToolStripButton();
      this.toolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
      this.buttonBreakAll = new System.Windows.Forms.ToolStripButton();
      this.buttonResumeAll = new System.Windows.Forms.ToolStripButton();
      this.buttonBreakCurrent = new System.Windows.Forms.ToolStripButton();
      this.toolStripSeparator2 = new System.Windows.Forms.ToolStripSeparator();
      this.m_statusStrip = new System.Windows.Forms.StatusStrip();
      this.m_splitContainerV = new System.Windows.Forms.SplitContainer();
      this.m_splitContainerH = new System.Windows.Forms.SplitContainer();
      this.m_scintilla = new ScintillaNet.Scintilla();
      this.m_output = new System.Windows.Forms.Panel();
      this.m_threads = new System.Windows.Forms.ListView();
      this.ThreadId = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
      this.Status = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
      this.m_callstack = new System.Windows.Forms.ListView();
      this.CallStack = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
      this.m_locals = new System.Windows.Forms.ListView();
      this.Variable = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
      this.Value = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
      this.m_menu.SuspendLayout();
      this.m_toolBar.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.m_splitContainerV)).BeginInit();
      this.m_splitContainerV.Panel1.SuspendLayout();
      this.m_splitContainerV.Panel2.SuspendLayout();
      this.m_splitContainerV.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.m_splitContainerH)).BeginInit();
      this.m_splitContainerH.Panel1.SuspendLayout();
      this.m_splitContainerH.Panel2.SuspendLayout();
      this.m_splitContainerH.SuspendLayout();
      ((System.ComponentModel.ISupportInitialize)(this.m_scintilla)).BeginInit();
      this.SuspendLayout();
      // 
      // m_menu
      // 
      this.m_menu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.fileToolStripMenuItem,
            this.helpToolStripMenuItem});
      this.m_menu.Location = new System.Drawing.Point(0, 0);
      this.m_menu.Name = "m_menu";
      this.m_menu.Size = new System.Drawing.Size(963, 24);
      this.m_menu.TabIndex = 0;
      this.m_menu.Text = "menuStrip1";
      // 
      // fileToolStripMenuItem
      // 
      this.fileToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.exitToolStripMenuItem});
      this.fileToolStripMenuItem.Name = "fileToolStripMenuItem";
      this.fileToolStripMenuItem.Size = new System.Drawing.Size(37, 20);
      this.fileToolStripMenuItem.Text = "File";
      // 
      // exitToolStripMenuItem
      // 
      this.exitToolStripMenuItem.Name = "exitToolStripMenuItem";
      this.exitToolStripMenuItem.Size = new System.Drawing.Size(92, 22);
      this.exitToolStripMenuItem.Text = "Exit";
      this.exitToolStripMenuItem.Click += new System.EventHandler(this.exitToolStripMenuItem_Click);
      // 
      // helpToolStripMenuItem
      // 
      this.helpToolStripMenuItem.DropDownItems.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.aboutToolStripMenuItem});
      this.helpToolStripMenuItem.Name = "helpToolStripMenuItem";
      this.helpToolStripMenuItem.Size = new System.Drawing.Size(44, 20);
      this.helpToolStripMenuItem.Text = "Help";
      // 
      // aboutToolStripMenuItem
      // 
      this.aboutToolStripMenuItem.Name = "aboutToolStripMenuItem";
      this.aboutToolStripMenuItem.Size = new System.Drawing.Size(199, 22);
      this.aboutToolStripMenuItem.Text = "About gmDebuggerNet";
      this.aboutToolStripMenuItem.Click += new System.EventHandler(this.aboutToolStripMenuItem_Click);
      // 
      // m_toolBar
      // 
      this.m_toolBar.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.buttonStepInto,
            this.buttonStepOver,
            this.buttonStepOut,
            this.buttonRun,
            this.buttonStopDebugging,
            this.buttonToggleBreakpoint,
            this.toolStripSeparator1,
            this.buttonBreakAll,
            this.buttonResumeAll,
            this.buttonBreakCurrent,
            this.toolStripSeparator2});
      this.m_toolBar.Location = new System.Drawing.Point(0, 24);
      this.m_toolBar.Name = "m_toolBar";
      this.m_toolBar.Size = new System.Drawing.Size(963, 25);
      this.m_toolBar.TabIndex = 1;
      this.m_toolBar.Text = "toolStrip1";
      // 
      // buttonStepInto
      // 
      this.buttonStepInto.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonStepInto.Image = ((System.Drawing.Image)(resources.GetObject("buttonStepInto.Image")));
      this.buttonStepInto.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonStepInto.Name = "buttonStepInto";
      this.buttonStepInto.Size = new System.Drawing.Size(23, 22);
      this.buttonStepInto.Text = "Step Into (F11)";
      this.buttonStepInto.Click += new System.EventHandler(this.buttonStepInto_Click);
      // 
      // buttonStepOver
      // 
      this.buttonStepOver.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonStepOver.Image = ((System.Drawing.Image)(resources.GetObject("buttonStepOver.Image")));
      this.buttonStepOver.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonStepOver.Name = "buttonStepOver";
      this.buttonStepOver.Size = new System.Drawing.Size(23, 22);
      this.buttonStepOver.Text = "Step Over (F10)";
      this.buttonStepOver.Click += new System.EventHandler(this.buttonStepOver_Click);
      // 
      // buttonStepOut
      // 
      this.buttonStepOut.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonStepOut.Image = ((System.Drawing.Image)(resources.GetObject("buttonStepOut.Image")));
      this.buttonStepOut.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonStepOut.Name = "buttonStepOut";
      this.buttonStepOut.Size = new System.Drawing.Size(23, 22);
      this.buttonStepOut.Text = "Step Out (shift + F11)";
      this.buttonStepOut.Click += new System.EventHandler(this.buttonStepOut_Click);
      // 
      // buttonRun
      // 
      this.buttonRun.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonRun.Image = ((System.Drawing.Image)(resources.GetObject("buttonRun.Image")));
      this.buttonRun.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonRun.Name = "buttonRun";
      this.buttonRun.Size = new System.Drawing.Size(23, 22);
      this.buttonRun.Text = "Run (F5)";
      this.buttonRun.Click += new System.EventHandler(this.buttonRun_Click);
      // 
      // buttonStopDebugging
      // 
      this.buttonStopDebugging.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonStopDebugging.Image = ((System.Drawing.Image)(resources.GetObject("buttonStopDebugging.Image")));
      this.buttonStopDebugging.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonStopDebugging.Name = "buttonStopDebugging";
      this.buttonStopDebugging.Size = new System.Drawing.Size(23, 22);
      this.buttonStopDebugging.Text = "Stop Debugging (shift + F5)";
      this.buttonStopDebugging.Click += new System.EventHandler(this.buttonStopDebugging_Click);
      // 
      // buttonToggleBreakpoint
      // 
      this.buttonToggleBreakpoint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonToggleBreakpoint.Image = ((System.Drawing.Image)(resources.GetObject("buttonToggleBreakpoint.Image")));
      this.buttonToggleBreakpoint.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonToggleBreakpoint.Name = "buttonToggleBreakpoint";
      this.buttonToggleBreakpoint.Size = new System.Drawing.Size(23, 22);
      this.buttonToggleBreakpoint.Text = "Toggle Breakpoint (F9)";
      this.buttonToggleBreakpoint.Click += new System.EventHandler(this.buttonToggleBreakpoint_Click);
      // 
      // toolStripSeparator1
      // 
      this.toolStripSeparator1.Name = "toolStripSeparator1";
      this.toolStripSeparator1.Size = new System.Drawing.Size(6, 25);
      // 
      // buttonBreakAll
      // 
      this.buttonBreakAll.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonBreakAll.Image = ((System.Drawing.Image)(resources.GetObject("buttonBreakAll.Image")));
      this.buttonBreakAll.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonBreakAll.Name = "buttonBreakAll";
      this.buttonBreakAll.Size = new System.Drawing.Size(23, 22);
      this.buttonBreakAll.Text = "Break All";
      this.buttonBreakAll.Click += new System.EventHandler(this.buttonBreakAll_Click);
      // 
      // buttonResumeAll
      // 
      this.buttonResumeAll.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonResumeAll.Image = ((System.Drawing.Image)(resources.GetObject("buttonResumeAll.Image")));
      this.buttonResumeAll.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonResumeAll.Name = "buttonResumeAll";
      this.buttonResumeAll.Size = new System.Drawing.Size(23, 22);
      this.buttonResumeAll.Text = "Resume All";
      this.buttonResumeAll.Click += new System.EventHandler(this.buttonResumeAll_Click);
      // 
      // buttonBreakCurrent
      // 
      this.buttonBreakCurrent.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image;
      this.buttonBreakCurrent.Image = ((System.Drawing.Image)(resources.GetObject("buttonBreakCurrent.Image")));
      this.buttonBreakCurrent.ImageTransparentColor = System.Drawing.Color.Magenta;
      this.buttonBreakCurrent.Name = "buttonBreakCurrent";
      this.buttonBreakCurrent.Size = new System.Drawing.Size(23, 22);
      this.buttonBreakCurrent.Text = "Break Current";
      this.buttonBreakCurrent.Click += new System.EventHandler(this.buttonBreakCurrent_Click);
      // 
      // toolStripSeparator2
      // 
      this.toolStripSeparator2.Name = "toolStripSeparator2";
      this.toolStripSeparator2.Size = new System.Drawing.Size(6, 25);
      // 
      // m_statusStrip
      // 
      this.m_statusStrip.Location = new System.Drawing.Point(0, 509);
      this.m_statusStrip.Name = "m_statusStrip";
      this.m_statusStrip.Size = new System.Drawing.Size(963, 22);
      this.m_statusStrip.TabIndex = 3;
      this.m_statusStrip.Text = "statusStrip1";
      // 
      // m_splitContainerV
      // 
      this.m_splitContainerV.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
      this.m_splitContainerV.Dock = System.Windows.Forms.DockStyle.Fill;
      this.m_splitContainerV.Location = new System.Drawing.Point(0, 49);
      this.m_splitContainerV.Name = "m_splitContainerV";
      // 
      // m_splitContainerV.Panel1
      // 
      this.m_splitContainerV.Panel1.Controls.Add(this.m_splitContainerH);
      // 
      // m_splitContainerV.Panel2
      // 
      this.m_splitContainerV.Panel2.Controls.Add(this.m_threads);
      this.m_splitContainerV.Panel2.Controls.Add(this.m_callstack);
      this.m_splitContainerV.Panel2.Controls.Add(this.m_locals);
      this.m_splitContainerV.Size = new System.Drawing.Size(963, 460);
      this.m_splitContainerV.SplitterDistance = 703;
      this.m_splitContainerV.TabIndex = 4;
      // 
      // m_splitContainerH
      // 
      this.m_splitContainerH.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
      this.m_splitContainerH.Dock = System.Windows.Forms.DockStyle.Fill;
      this.m_splitContainerH.Location = new System.Drawing.Point(0, 0);
      this.m_splitContainerH.Name = "m_splitContainerH";
      this.m_splitContainerH.Orientation = System.Windows.Forms.Orientation.Horizontal;
      // 
      // m_splitContainerH.Panel1
      // 
      this.m_splitContainerH.Panel1.Controls.Add(this.m_scintilla);
      // 
      // m_splitContainerH.Panel2
      // 
      this.m_splitContainerH.Panel2.Controls.Add(this.m_output);
      this.m_splitContainerH.Size = new System.Drawing.Size(703, 460);
      this.m_splitContainerH.SplitterDistance = 364;
      this.m_splitContainerH.TabIndex = 0;
      // 
      // m_scintilla
      // 
      this.m_scintilla.Dock = System.Windows.Forms.DockStyle.Fill;
      this.m_scintilla.Location = new System.Drawing.Point(0, 0);
      this.m_scintilla.Name = "m_scintilla";
      this.m_scintilla.Size = new System.Drawing.Size(699, 360);
      this.m_scintilla.Styles.BraceBad.FontName = "Verdana";
      this.m_scintilla.Styles.BraceLight.FontName = "Verdana";
      this.m_scintilla.Styles.ControlChar.FontName = "Verdana";
      this.m_scintilla.Styles.Default.FontName = "Verdana";
      this.m_scintilla.Styles.IndentGuide.FontName = "Verdana";
      this.m_scintilla.Styles.LastPredefined.FontName = "Verdana";
      this.m_scintilla.Styles.LineNumber.FontName = "Verdana";
      this.m_scintilla.Styles.Max.FontName = "Verdana";
      this.m_scintilla.TabIndex = 0;
      // 
      // m_output
      // 
      this.m_output.Dock = System.Windows.Forms.DockStyle.Fill;
      this.m_output.Location = new System.Drawing.Point(0, 0);
      this.m_output.Name = "m_output";
      this.m_output.Size = new System.Drawing.Size(699, 88);
      this.m_output.TabIndex = 0;
      // 
      // m_threads
      // 
      this.m_threads.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ThreadId,
            this.Status});
      this.m_threads.Dock = System.Windows.Forms.DockStyle.Fill;
      this.m_threads.FullRowSelect = true;
      this.m_threads.GridLines = true;
      this.m_threads.Location = new System.Drawing.Point(0, 296);
      this.m_threads.MultiSelect = false;
      this.m_threads.Name = "m_threads";
      this.m_threads.Size = new System.Drawing.Size(252, 160);
      this.m_threads.TabIndex = 2;
      this.m_threads.UseCompatibleStateImageBehavior = false;
      this.m_threads.View = System.Windows.Forms.View.Details;
      this.m_threads.Click += new System.EventHandler(this.m_threads_Click);
      // 
      // ThreadId
      // 
      this.ThreadId.Tag = "";
      this.ThreadId.Text = "ThreadId";
      this.ThreadId.Width = 78;
      // 
      // Status
      // 
      this.Status.Tag = "";
      this.Status.Text = "Status";
      this.Status.Width = 54;
      // 
      // m_callstack
      // 
      this.m_callstack.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.CallStack});
      this.m_callstack.Dock = System.Windows.Forms.DockStyle.Top;
      this.m_callstack.GridLines = true;
      this.m_callstack.Location = new System.Drawing.Point(0, 123);
      this.m_callstack.MultiSelect = false;
      this.m_callstack.Name = "m_callstack";
      this.m_callstack.Size = new System.Drawing.Size(252, 173);
      this.m_callstack.TabIndex = 1;
      this.m_callstack.UseCompatibleStateImageBehavior = false;
      this.m_callstack.View = System.Windows.Forms.View.Details;
      // 
      // CallStack
      // 
      this.CallStack.Text = "Call Stack";
      this.CallStack.Width = 80;
      // 
      // m_locals
      // 
      this.m_locals.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.Variable,
            this.Value});
      this.m_locals.Dock = System.Windows.Forms.DockStyle.Top;
      this.m_locals.GridLines = true;
      this.m_locals.Location = new System.Drawing.Point(0, 0);
      this.m_locals.MultiSelect = false;
      this.m_locals.Name = "m_locals";
      this.m_locals.Size = new System.Drawing.Size(252, 123);
      this.m_locals.TabIndex = 0;
      this.m_locals.UseCompatibleStateImageBehavior = false;
      this.m_locals.View = System.Windows.Forms.View.Details;
      // 
      // Variable
      // 
      this.Variable.Text = "Variable";
      this.Variable.Width = 65;
      // 
      // Value
      // 
      this.Value.Text = "Value";
      this.Value.Width = 50;
      // 
      // MainForm
      // 
      this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
      this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
      this.ClientSize = new System.Drawing.Size(963, 531);
      this.Controls.Add(this.m_splitContainerV);
      this.Controls.Add(this.m_statusStrip);
      this.Controls.Add(this.m_toolBar);
      this.Controls.Add(this.m_menu);
      this.MainMenuStrip = this.m_menu;
      this.MinimumSize = new System.Drawing.Size(512, 512);
      this.Name = "MainForm";
      this.Text = "gmDebuggerNet";
      this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.MainForm_FormClosing);
      this.Load += new System.EventHandler(this.MainForm_Load);
      this.SizeChanged += new System.EventHandler(this.MainForm_SizeChanged);
      this.m_menu.ResumeLayout(false);
      this.m_menu.PerformLayout();
      this.m_toolBar.ResumeLayout(false);
      this.m_toolBar.PerformLayout();
      this.m_splitContainerV.Panel1.ResumeLayout(false);
      this.m_splitContainerV.Panel2.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.m_splitContainerV)).EndInit();
      this.m_splitContainerV.ResumeLayout(false);
      this.m_splitContainerH.Panel1.ResumeLayout(false);
      this.m_splitContainerH.Panel2.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.m_splitContainerH)).EndInit();
      this.m_splitContainerH.ResumeLayout(false);
      ((System.ComponentModel.ISupportInitialize)(this.m_scintilla)).EndInit();
      this.ResumeLayout(false);
      this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.MenuStrip m_menu;
    private System.Windows.Forms.ToolStripMenuItem fileToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem exitToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem helpToolStripMenuItem;
    private System.Windows.Forms.ToolStripMenuItem aboutToolStripMenuItem;
    private System.Windows.Forms.ToolStrip m_toolBar;
    private System.Windows.Forms.StatusStrip m_statusStrip;
    private System.Windows.Forms.SplitContainer m_splitContainerV;
    private System.Windows.Forms.SplitContainer m_splitContainerH;
    private System.Windows.Forms.ToolStripButton buttonStepInto;
    private System.Windows.Forms.ToolStripButton buttonStepOver;
    private System.Windows.Forms.ToolStripButton buttonStepOut;
    private System.Windows.Forms.ToolStripButton buttonRun;
    private System.Windows.Forms.ToolStripButton buttonStopDebugging;
    private System.Windows.Forms.ToolStripButton buttonToggleBreakpoint;
    private System.Windows.Forms.ToolStripSeparator toolStripSeparator1;
    private System.Windows.Forms.ToolStripButton buttonBreakAll;
    private System.Windows.Forms.ToolStripButton buttonResumeAll;
    private System.Windows.Forms.ToolStripButton buttonBreakCurrent;
    private System.Windows.Forms.ToolStripSeparator toolStripSeparator2;
    private ScintillaNet.Scintilla m_scintilla;
    private System.Windows.Forms.ListView m_threads;
    private System.Windows.Forms.ListView m_callstack;
    private System.Windows.Forms.ListView m_locals;
    private System.Windows.Forms.ColumnHeader CallStack;
    private System.Windows.Forms.ColumnHeader Variable;
    private System.Windows.Forms.ColumnHeader Value;
    private System.Windows.Forms.ColumnHeader ThreadId;
    private System.Windows.Forms.ColumnHeader Status;
    private System.Windows.Forms.Panel m_output;
  }
}

