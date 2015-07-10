using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace gmDebuggerNet
{
  public partial class MainForm : Form
  {
    public App m_app;
    public Timer m_tick;

    public MainForm()
    {
      InitializeComponent();

      // Allow form to receive keyboard focus
      CreateControl();
      SetStyle(ControlStyles.Selectable, true);
      this.UpdateStyles();

      // Allow us to process some shortcut keys instead of Scintilla
      m_scintilla.KeyDown += new KeyEventHandler(ScintillaKeyDown);

      // Create app instance
      m_app = new App();
      m_app.Init(this);

      // Update tick
      m_tick = new Timer();
      m_tick.Tick += new EventHandler(m_app.OnTick);
      m_tick.Interval = 1; //Interval in MS
      m_tick.Start();
    }

    public System.Windows.Forms.ListView GetLocals() { return m_locals; }
    public System.Windows.Forms.ListView GetThreads() { return m_threads; }
    public System.Windows.Forms.ListView GetCallStack() { return m_callstack; }
    public ScintillaNet.Scintilla GetSourceView() { return m_scintilla; }
    public System.Windows.Forms.Panel GetOutput() { return m_output; }

    public void UpdateToolBar()
    {
      bool hasCurrentThread = false;
      bool isDebugging = false;

      if( m_app.m_state.m_currentDebugThread != 0 )
      {
        hasCurrentThread = true;
      }
      if( m_app.m_state.m_isDebugging )
      {
        isDebugging = true;
      }

      buttonStepInto.Enabled = hasCurrentThread;
      buttonStepOver.Enabled = hasCurrentThread;
      buttonStepOut.Enabled = hasCurrentThread;
      buttonRun.Enabled = hasCurrentThread;
      buttonToggleBreakpoint.Enabled = hasCurrentThread;
      buttonBreakCurrent.Enabled = hasCurrentThread;

      buttonStopDebugging.Enabled = isDebugging;
      buttonBreakAll.Enabled = isDebugging;
      buttonResumeAll.Enabled = isDebugging;
    }

    private void exitToolStripMenuItem_Click(object sender, EventArgs e)
    {
      Close();
    }

    private void aboutToolStripMenuItem_Click(object sender, EventArgs e)
    {
      DialogResult result = MessageBox.Show(this, 
                                            "Example debugger for GameMonkey Script", 
                                            "About gmDebuggerNet",
                                            MessageBoxButtons.OK,
                                            MessageBoxIcon.Information,
                                            MessageBoxDefaultButton.Button1);

    }

    private void m_threads_Click(object sender, EventArgs e)
    {
      if (m_threads.SelectedItems.Count > 0)
      {
        ListViewItem selected = m_threads.SelectedItems[0];
        if ((selected != null) && (selected.Tag != null))
        {
          m_app.OnGetThreadContext( (DebuggerState.ThreadInfo)selected.Tag );
        }
      }
    }

    private void buttonResumeAll_Click(object sender, EventArgs e)
    {
      m_app.OnResumeAll();
    }

    private void buttonBreakAll_Click(object sender, EventArgs e)
    {
      m_app.OnBreakAll();
    }

    private void buttonStepInto_Click(object sender, EventArgs e)
    {
      m_app.OnStepInto();
    }

    private void buttonStepOver_Click(object sender, EventArgs e)
    {
      m_app.OnStepOver();
    }

    private void buttonStepOut_Click(object sender, EventArgs e)
    {
      m_app.OnStepOut();
    }

    private void buttonRun_Click(object sender, EventArgs e)
    {
      m_app.OnRun();
    }

    private void buttonStopDebugging_Click(object sender, EventArgs e)
    {
      m_app.OnStopDebugging();
    }

    private void buttonToggleBreakpoint_Click(object sender, EventArgs e)
    {
      m_app.OnToggleBreakpoint();
    }

    private void buttonBreakCurrent_Click(object sender, EventArgs e)
    {
      m_app.OnBreakCurrentThread();
    }

    // Still need to handle keys from Scintilla
    void ScintillaKeyDown(object sender, KeyEventArgs e)
    {
      OnKeyDown(e);
    }

    // Overriding this to grab keys as early as possible
    protected override bool ProcessDialogKey(	Keys a_keyData )
    {
      KeyEventArgs keyArgs = new KeyEventArgs(a_keyData);

      this.OnKeyDown( keyArgs );

      if(keyArgs.Handled)
      {
        return true;
      }

      return base.ProcessDialogKey(a_keyData);
    } 

    //private void MainForm_KeyDown(object sender, KeyEventArgs e)
    protected override void OnKeyDown(KeyEventArgs e)
    {
      bool processInput = true;

      if (e.Shift == true )
      {
        if( e.KeyCode == Keys.F11 )
        {
          buttonStepOut_Click(null, null);
          processInput = false;
        }
        else if (e.KeyCode == Keys.F5)
        {
          buttonStopDebugging_Click(null, null);
          processInput = false;
        }
      }
      else
      {
        if (e.KeyCode == Keys.F11)
        {
          buttonStepInto_Click(null, null);
          processInput = false;
        }
        else if (e.KeyCode == Keys.F10)
        {
          buttonStepOver_Click(null, null);
          processInput = false;
        }
        else if (e.KeyCode == Keys.F5)
        {
          buttonRun_Click(null, null);
          processInput = false;
        }
        else if (e.KeyCode == Keys.F9)
        {
          buttonToggleBreakpoint_Click(null, null);
          processInput = false;
        }
        else if (e.KeyCode == Keys.F8)
        {
          buttonBreakCurrent_Click(null, null);
          processInput = false;
        }
      }

      if( !processInput )
      {
        e.Handled = true;
      }

      base.OnKeyDown(e);
    }

    private void MainForm_FormClosing(object sender, FormClosingEventArgs e)
    {
      if( m_app != null )
      {
        m_app.OnExit();
      }
    }

    private void MainForm_SizeChanged(object sender, EventArgs e)
    {
      ResizeComponents();
    }

    public void ResizeComponents()
    {
      // Set column widths based on number of columns
      ResizeListViewColumns(m_locals);
      ResizeListViewColumns(m_threads);
      ResizeListViewColumns(m_callstack);

      // Spread panels vertically
      int totalHeight = m_splitContainerV.Panel2.Height;
      int panelHeight = totalHeight / 3;
      if (panelHeight < 10) // Paranoid clamp size
      {
        panelHeight = 10;
      }

      m_locals.Height = panelHeight;
      m_callstack.Height = panelHeight;
      m_threads.Height = panelHeight;
    }

    public void ResizeListViewColumns(ListView a_listView)
    {
      int numColumns = a_listView.Columns.Count;
      int columnWidth = (m_locals.Width - 2) / numColumns;
      if (columnWidth < 10) // Paranoid clamp size
      {
        columnWidth = 10;
      }
      
      for(int colIndex=0; colIndex < a_listView.Columns.Count; ++colIndex)
      {
        a_listView.Columns[colIndex].Width = columnWidth;
      }
    }

    private void MainForm_Load(object sender, EventArgs e)
    {
      ResizeComponents();
    }

  }
}
