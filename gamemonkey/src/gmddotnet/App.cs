using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Drawing;

// TODO
//
// o Handle 64bit GM builds, needs to unpack 64bit gmptr for some messages (which ones?)
//   Probably need to send a machine info message to let us know what kind of GM machine we are connecting to.
//
// o This whole application is a C# .Net conversion from the original GMD and it inherits all the (terrible) existing behavior.
//   A serious debugger needs to rework the connection method and greatly improve usability.
//

namespace gmDebuggerNet
{
  // Central application logic class
  public class App
  {
    // NOTE: MSVC will look for Scilexer.dll in the search path for form designer.  
    //       Simplest work around is to copy the file to Windows\SysWOW64 (or Windows\System32 for 32bit OS)
    //       I don't know why it worked without doing this the first time.

    public DebuggerSession m_debuggerSession;               // Debugger session for messaging
    public OurDebuggerRecInterface m_debuggerRecInterface;  // Interface to handle rec messages
    public Network m_network;                               // Network connection
    public DebuggerState m_state;                           // Debugger state
    public MainForm m_mainForm;                             // Main form
    public OutputWin m_outputWin;                           // Log window
    protected long m_timeLastPollMachineStateMS;            // Time since last machine state poll in MS
    public ScintillaNet.Marker m_markerBreakpoint;          // Break point marker
    public ScintillaNet.Marker m_markerArrow;               // Current execution point marker

    // Constructor
    public App()
    {
      m_network = new Network();
      m_debuggerSession = new DebuggerSession(m_network);
      m_debuggerRecInterface = new OurDebuggerRecInterface(this);
      m_state = new DebuggerState();

      m_timeLastPollMachineStateMS = 0;
    }

    public bool Init(MainForm a_mainForm)
    {
      int port = 49001;

      m_mainForm = a_mainForm;
      
      m_outputWin = new OutputWin();

      m_outputWin.GetControl().Parent = m_mainForm.GetOutput();
      m_outputWin.GetControl().Dock = DockStyle.Fill;
      m_outputWin.SetLineLimit(20);

      InitScintilla();

      if (!m_network.Open(port))
      {
        return false;
      }

      m_outputWin.AddLogText(OutputWin.LogType.Info, "Listening on port '" + port.ToString() + "'");

      return true;
    }

    public void InitScintilla()
    {
      ScintillaNet.Scintilla sourceView = m_mainForm.GetSourceView();
      
      sourceView.Margins[0].Width = 20; // Show line numbers
      sourceView.IsReadOnly = true; // Disable editing
      
      sourceView.ConfigurationManager.Language = "cpp";
      sourceView.EndOfLine.Mode = ScintillaNet.EndOfLineMode.Crlf;

      m_markerBreakpoint = sourceView.Markers[0];
      m_markerBreakpoint.Symbol = ScintillaNet.MarkerSymbol.RoundRectangle;
      m_markerBreakpoint.ForeColor = Color.Black;
      m_markerBreakpoint.BackColor = Color.Red;

      m_markerArrow = sourceView.Markers[1];
      m_markerArrow.Symbol = ScintillaNet.MarkerSymbol.Arrow;
      m_markerArrow.ForeColor = Color.Black;
      m_markerArrow.BackColor = Color.Blue;
    }

    public void OnTick(object a_sender, EventArgs a_eventArgs)
    {
      if( m_network.IsConnected )
      {
        m_state.m_isDebugging = true;

        // Process incomming net messages
        m_mainForm.SuspendLayout(); // Attempt to speed up UI which will be updated by incoming net messages
        m_debuggerSession.Update(m_debuggerRecInterface);
        m_mainForm.ResumeLayout();

        // Periodically grab the thread states
        const long POLL_MACHINE_RATE = 5000; // 5 second
        long currentTimeMS = DateTime.Now.Ticks / 10000; // Convert 100ns to MS
        long deltaTime = currentTimeMS - m_timeLastPollMachineStateMS;
        if( deltaTime >= POLL_MACHINE_RATE )
        {
          //m_outputWin.AddLogText(OutputWin.LogType.Info, "poll" + deltaTime.ToString());
          m_timeLastPollMachineStateMS = currentTimeMS; 
          
          // Poll for thread changes
          m_debuggerSession.gmMachineGetThreadInfo();
        }
      }
      else if( m_state.m_isDebugging )
      {
        m_state.m_isDebugging = false;
      }

      m_mainForm.UpdateToolBar();

      if( (m_network != null ) && (m_network.m_closeStatus == Network.CloseStatus.LostConnection) )
      {
        m_network.ClearCloseStatus();
        m_outputWin.AddLogText(OutputWin.LogType.Info, "Lost connection");
      }
    }

    public void Disconnect()
    {
      m_mainForm.GetThreads().Items.Clear();
      ClearCurrentContext();

      if( m_network.IsConnected )
      {
        m_network.Done();
      }

      m_state.Clear();
    }

    static public ListViewItem MakeListViewItem(String a_label, Object a_item, Object a_user)
    {
      ListViewItem newItem = new ListViewItem();
      newItem.Text = a_label;
      newItem.SubItems.Add(a_item.ToString());
      newItem.Tag = a_user;
      return newItem;
    }


    public void ClearCurrentContext()
    {
      /* What was the point of this, I think to clear focus  
            int sel = -1;
            ListView listViewThreads = m_mainForm.GetThreads();
            if( listViewThreads.SelectedIndices.Count > 0 )
            {
              sel = listViewThreads.SelectedIndices[0];
              //ORIGINAL m_threadsWindow.SetItemState(sel, 0, LVIS_SELECTED | LVIS_FOCUSED);
            }
      */

      m_state.m_currentDebugThread = 0;

      // Unselect thread
      ListView listViewThreads = m_mainForm.GetThreads();
      foreach(ListViewItem item in listViewThreads.Items)
      {
        item.Selected = false;
      }
      m_state.m_currentPos = -1;

      // clear the windows
      SetSource(0, ""); // hackorama
      m_mainForm.GetLocals().Items.Clear();
      m_mainForm.GetCallStack().Items.Clear();
    }

    public void RemoveThread(int a_threadId)
    {
      if (a_threadId <= 0)
      {
        return;
      }

      for(int thIndex=0; thIndex < m_state.m_threads.Count; ++thIndex)
      {
        if( m_state.m_threads[thIndex].m_id == a_threadId )
        {
          ListView listViewThreads = m_mainForm.GetThreads();
          for(int itemIndex=0; itemIndex < listViewThreads.Items.Count; ++itemIndex)
          {
            if( ((DebuggerState.ThreadInfo)listViewThreads.Items[itemIndex].Tag).m_id == a_threadId )
            {
              listViewThreads.Items.RemoveAt(itemIndex);
              break;
            }
          }
          
          m_state.m_threads.RemoveAt(thIndex);
          break;
        }
      }
    }

    public bool SetSource(uint a_sourceId, String a_source)
    {
      if (a_sourceId == m_state.m_sourceId)
      {
        return true;
      }

      ScintillaNet.Scintilla sourceView = m_mainForm.GetSourceView();

      // do we have the source
      foreach (DebuggerState.Source source in m_state.m_sources)
      {
        if (source.m_id == a_sourceId)
        {
          sourceView.IsReadOnly = false;
          sourceView.Text = source.m_source;
          sourceView.IsReadOnly = true;
          m_state.m_sourceId = a_sourceId;
          return true;
        }
      }

      // we dont have the source, add it
      if (a_source != null)
      {
        sourceView.IsReadOnly = false;
        sourceView.Text = a_source;
        sourceView.IsReadOnly = true;
        m_state.m_sourceId = a_sourceId;
        m_state.m_sources.Add(new DebuggerState.Source(a_source, a_sourceId));
        return true;
      }

      return false;
    }

    public void SetLine(int a_line)
    {
      ScintillaNet.Scintilla sourceView = m_mainForm.GetSourceView();

      if (m_state.m_currentPos >= 0)
      {
        sourceView.Lines[m_state.m_currentPos].DeleteMarker(m_markerArrow);
        m_state.m_currentPos = -1;
      }
      m_state.m_currentPos = a_line - 1;
      sourceView.Lines[m_state.m_currentPos].AddMarker(m_markerArrow);

      // center the source view around the cursor
      int topLine = sourceView.Lines.FirstVisible.Number;
      int visLines = sourceView.Lines.VisibleCount;
      int scrollLines = 0;
      int centre = (topLine + (visLines >> 1));
      int lq = centre - (visLines >> 2);
      int hq = centre + (visLines >> 2);

      if (m_state.m_currentPos < lq)
      {
        scrollLines = m_state.m_currentPos - centre;
      }
      else if (m_state.m_currentPos > hq)
      {
        scrollLines = m_state.m_currentPos - centre;
      }
      // LineScroll(0, scrollLines);
      sourceView.Scrolling.ScrollBy(0, scrollLines);
    }

    public static String GetThreadStateAsString(int a_stateId)
    {
      String stateDesc = "unknown";
      switch (a_stateId)
      {
        case 0: stateDesc = "running"; break;
        case 1: stateDesc = "blocked"; break;
        case 2: stateDesc = "sleeping"; break;
        case 3: stateDesc = "exception"; break;
        case 4: stateDesc = "broken"; break;
        default: break;
      }

      return stateDesc;
    }

    public void UpdateThreadWindow()
    {
      // Update display from data
      ListView listView = m_mainForm.GetThreads();
      listView.Items.Clear();

      for (int threadIndex = 0; threadIndex < m_state.m_threads.Count; ++threadIndex)
      {
        DebuggerState.ThreadInfo curThreadInfo = m_state.m_threads[threadIndex];

        String stateDesc = App.GetThreadStateAsString(curThreadInfo.m_state);

        listView.Items.Add(App.MakeListViewItem(curThreadInfo.m_id.ToString(), stateDesc, curThreadInfo));
      }
    }

    public void FindAddThread(int a_threadId, int a_state, bool a_select)
    {
      if (a_threadId <= 0)
      {
        return;
      }

      ListView listView = m_mainForm.GetThreads();

      // Clear selection
      if (a_select)
      {
        foreach (ListViewItem item in listView.Items)
        {
          item.Selected = false;
        }
      }

      // find in list
      for (int thIndex = 0; thIndex < m_state.m_threads.Count; ++thIndex)
      {
        DebuggerState.ThreadInfo info = m_state.m_threads[thIndex];

        if (info.m_id == a_threadId)
        {
          // update state
          if (info.m_state != a_state || a_select)
          {
            if (info.m_state != a_state)
            {
              info.m_state = a_state;
            }

            for (int itemIndex = 0; itemIndex < listView.Items.Count; ++itemIndex)
            {
              if (listView.Items[itemIndex].Tag == info)
              {
                String stateDesc = GetThreadStateAsString(info.m_state);
                listView.Items[itemIndex] = App.MakeListViewItem(info.m_id.ToString(), stateDesc, info);

                if (a_select)
                {
                  listView.Items[itemIndex].Selected = true;
                }
                break;
              }
            }
          }
          return;
        }
      }

      // add
      {
        DebuggerState.ThreadInfo info = new DebuggerState.ThreadInfo(a_threadId, a_state);
        m_state.m_threads.Add(info);

        String stateDesc = GetThreadStateAsString(info.m_state);
        listView.Items.Insert(0, App.MakeListViewItem(info.m_id.ToString(), stateDesc, info));

        if (a_select)
        {
          listView.Items[0].Selected = true;
        }
      }
    }

    public void OnExit()
    {
      if( m_debuggerSession != null )
      {
        m_debuggerSession.gmMachineQuit();
      }
      System.Threading.Thread.Sleep(500); // wait for quit message to post (hack)
      Disconnect();
    }

    public void OnGetThreadContext(DebuggerState.ThreadInfo a_threadInfo)
    {
      m_debuggerSession.gmMachineGetContext(a_threadInfo.m_id, 0);
    }

    public void OnResumeAll()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        foreach (DebuggerState.ThreadInfo threadInfo in m_state.m_threads)
        {
          // send run command
          m_debuggerSession.gmMachineRun(threadInfo.m_id);
        }
      }
    }

    public void OnBreakAll()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        foreach (DebuggerState.ThreadInfo threadInfo in m_state.m_threads)
        {
          // send break command
          m_debuggerSession.gmMachineBreak(threadInfo.m_id);
        }
      }
    }

    public void OnStepInto()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        m_debuggerSession.gmMachineStepInto(m_state.m_currentDebugThread);
      }
    }

    public void OnStepOut()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        m_debuggerSession.gmMachineStepOut(m_state.m_currentDebugThread);
      }
    }

    public void OnStepOver()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        m_debuggerSession.gmMachineStepOver(m_state.m_currentDebugThread);
      }
    }

    public void OnRun()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        m_debuggerSession.gmMachineRun(m_state.m_currentDebugThread);
      }
    }

    public void OnBreakCurrentThread() 
    {
      if( m_state.m_currentDebugThread != 0 )
      {
        m_debuggerSession.gmMachineBreak(m_state.m_currentDebugThread);
      }
    }

    public void OnStopDebugging()
    {
      if (m_network.IsConnected && m_state.m_isDebugging)
      {
        m_debuggerSession.gmMachineQuit();
        System.Threading.Thread.Sleep(500); // wait for quit message to post (hack)
        Disconnect();
      }
    }

    public void OnToggleBreakpoint() 
    {
      ScintillaNet.Scintilla sourceView = m_mainForm.GetSourceView();

      // add a break point at the current line.
      int line = sourceView.Lines.Current.Number; // m_scintillaEdit.GetCurrentPos();
      bool enabled = true;

      // do we have a break point at this line already???
      if (sourceView.Lines[line].GetMarkers().Contains(m_markerBreakpoint))
      {
        enabled = false;
      }

      if( m_state.m_currentDebugThread != 0 && (m_state.m_isDebugging != false) )
      {
        m_state.m_breakPoint.m_enabled = enabled;
        m_state.m_breakPoint.m_allThreads = true;
        m_state.m_breakPoint.m_responseId = ++m_state.m_responseId;
        m_state.m_breakPoint.m_sourceId = m_state.m_sourceId;
        m_state.m_breakPoint.m_lineNumber = line + 1;
        m_state.m_breakPoint.m_threadId = 0;

        m_debuggerSession.gmMachineSetBreakPoint(m_state.m_breakPoint.m_responseId,
                                                 (int)m_state.m_breakPoint.m_sourceId, 
                                                 m_state.m_breakPoint.m_lineNumber, 
                                                 0, 
                                                 enabled ? 1 : 0);
      }
    }


  }

  // Handle rec messages
  public class OurDebuggerRecInterface : DebuggerRecInterface
  {
    App m_app;

    // Constructor
    public OurDebuggerRecInterface(App a_app)
    {
      m_app = a_app;
    }

    public void gmDebuggerBreak(int a_threadId, int a_sourceId, int a_lineNumber)
    {
      m_app.m_debuggerSession.gmMachineGetContext(a_threadId, 0);
    }

    public void gmDebuggerRun(int a_threadId)
    {
      String text = "thread " + a_threadId.ToString() + " started.";
      m_app.m_outputWin.AddLogText(OutputWin.LogType.Info, text);
      m_app.FindAddThread(a_threadId, 0, false);
    }

    public void gmDebuggerStop(int a_threadId)
    {
      String text = "thread " + a_threadId.ToString() + " stopped.";
      m_app.m_outputWin.AddLogText(OutputWin.LogType.Info, text);
      if( a_threadId == m_app.m_state.m_currentDebugThread )
      {
        m_app.ClearCurrentContext();
      }
      m_app.RemoveThread(a_threadId);
    }

    public void gmDebuggerSource(int a_sourceId, String a_sourceName, String a_source)
    {
      m_app.SetSource((uint)a_sourceId, a_source);
      if( m_app.m_state.m_lineNumberOnSourceRcv != -1)
      {
        m_app.SetLine(m_app.m_state.m_lineNumberOnSourceRcv);
        m_app.m_state.m_lineNumberOnSourceRcv = -1;
      }
    }

    public void gmDebuggerException(int a_threadId)
    {
      System.Console.Beep();
      m_app.m_debuggerSession.gmMachineGetContext(a_threadId, 0);
    }
    
    
    public void gmDebuggerBeginContext(int a_threadId, int a_callFrame)
    {
      m_app.m_state.m_currentDebugThread = a_threadId;
      m_app.FindAddThread(m_app.m_state.m_currentDebugThread, 4, true);
      m_app.m_state.m_currentCallFrame = a_callFrame;
       
      m_app.m_mainForm.GetLocals().Items.Clear();
      m_app.m_mainForm.GetCallStack().Items.Clear();
    }

    public void gmDebuggerContextCallFrame(int a_callFrame, String a_functionName, int a_sourceId, int a_lineNumber, String a_thisSymbol, String a_thisValue, int a_thisId)
    {
      ListView listViewCS = m_app.m_mainForm.GetCallStack();

      String functionDesc = a_functionName + " (" + a_lineNumber.ToString() + ")";
      listViewCS.Items.Add(new ListViewItem(functionDesc));
       
      if( m_app.m_state.m_currentCallFrame == a_callFrame )
      {
        ListView listViewLocals = m_app.m_mainForm.GetLocals();

        // add "this"
        int index = listViewLocals.Items.Count;
        listViewLocals.Items.Insert(index, App.MakeListViewItem(a_thisSymbol, a_thisValue, null) );

        // do we have the source code?
        m_app.m_state.m_lineNumberOnSourceRcv = -1;
        if (!m_app.SetSource((uint)a_sourceId, null))
        {
          // request source
          m_app.m_debuggerSession.gmMachineGetSource(a_sourceId);
          m_app.m_state.m_lineNumberOnSourceRcv = a_lineNumber;
        }
        else
        {
          // update the position cursor
          m_app.SetLine(a_lineNumber);
        }
      }
    }

    public void gmDebuggerContextVariable(String a_varSymbol, String a_varValue, int a_varId)
    {
      ListView listViewLocals = m_app.m_mainForm.GetLocals();
      listViewLocals.Items.Add( App.MakeListViewItem(a_varSymbol, a_varValue, null) );
    }

    public void gmDebuggerEndContext()
    {
      // Do nothing
    }

    public void gmDebuggerBeginSourceInfo()
    {
      // Do nothing
    }

    public void gmDebuggerSourceInfo(int a_sourceId, String a_sourceName)
    {
      // Do nothing
    }

    public void gmDebuggerEndSourceInfo()
    {
      // Do nothing
    }

    public void gmDebuggerBeginThreadInfo()
    {
      // Clear data
      m_app.m_state.m_threads.Clear();
    }

    public void gmDebuggerThreadInfo(int a_threadId, int a_threadState)
    {
      // Fill data
      m_app.m_state.m_threads.Add( new DebuggerState.ThreadInfo(a_threadId, a_threadState) );
    }
    
    public void gmDebuggerEndThreadInfo()
    {
      // Update display from data
      m_app.UpdateThreadWindow();
    }

    public void gmDebuggerError(String a_error)
    {
      m_app.m_outputWin.AddLogText(OutputWin.LogType.Error, a_error);
    }

    public void gmDebuggerMessage(String a_message)
    {
      m_app.m_outputWin.AddLogText(OutputWin.LogType.Info, a_message);
    }

    public void gmDebuggerAck(int a_response, int a_posNeg)
    {
      if( (a_response == m_app.m_state.m_responseId) && (a_posNeg != 0) )
      {
        ScintillaNet.Scintilla sourceView = m_app.m_mainForm.GetSourceView();

        if( m_app.m_state.m_breakPoint.m_enabled )
        {
          sourceView.Lines[m_app.m_state.m_breakPoint.m_lineNumber - 1].AddMarker( m_app.m_markerBreakpoint );
        }
        else
        {
          sourceView.Lines[m_app.m_state.m_breakPoint.m_lineNumber - 1].DeleteMarker(m_app.m_markerBreakpoint);
        }
      }
    }

    public void gmDebuggerQuit()
    {
      m_app.m_outputWin.AddLogText(OutputWin.LogType.Info, "Disconnected");
      m_app.Disconnect();
    }

  }
}
