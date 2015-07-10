using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.Drawing;


namespace gmDebuggerNet
{
  // Quick and dirty log window
  // Uses RichTextBox to store a limited set of colored lines of text
  public class OutputWin
  {
    public enum LogType
    {
      Info,
      Warning,
      Error,
    };

    protected RichTextBox m_control;
    protected Int32 m_maxLines;
    protected Color m_colorError;
    protected Color m_colorWarning;
    protected Color m_colorInfo;
 
    public OutputWin()
    {
      m_control = null;

      m_colorError = Color.Red;
      m_colorWarning = Color.OrangeRed;
      m_colorInfo = Color.Black;

      m_maxLines = 200;

      m_control = new RichTextBox();
      m_control.ReadOnly = true;
    }

    public Control GetControl()                           { return m_control; }

    public void AddLogText(LogType a_logType, String a_text)
    {
      if( m_control == null )
      {
        return;
      }
  
      switch (a_logType)
      {
        case LogType.Error:
        {
          FormsUtils.RTBAppendLine(m_control, a_text, m_colorError);
          break;
        }
        case LogType.Warning:
        {
          FormsUtils.RTBAppendLine(m_control, a_text, m_colorWarning);
          break;
        }
        default: // LogType.Info
        {
          FormsUtils.RTBAppendLine(m_control, a_text, m_colorInfo);
          break;
        }
      }
  
      FormsUtils.RTBScrollToEnd(m_control); // Do we want to do this?

      TrimToLineLimit();    
    }


    public void TrimToLineLimit()
    {
      if( m_control == null )
      {
        return;
      }
      while( FormsUtils.RTBGetNumLines(m_control) > m_maxLines )
      {
        FormsUtils.RTBDeleteFirstLine(m_control);
      }
    }

    public void SetLineLimit(int a_maxLines)
    {
      m_maxLines = a_maxLines;
      if( m_control != null )
      {
        TrimToLineLimit();
      }
    }

    public void Clear()
    {
      if( m_control != null )
      {
        m_control.Clear();
      }
    }

  }

  // Helpers to work with RichTextBox control
  // Pasted from another project
  public class FormsUtils
  {
    static public void RTBSelectLine(RichTextBox a_rtb, int a_line)
    {
      int start = a_rtb.GetFirstCharIndexFromLine(a_line);
      int end = a_rtb.GetFirstCharIndexFromLine(a_line + 1);

      if( start < 0 )
      {
        start = 0;
        end = 0;
      }
      if( end < 0 )
      {
        end = a_rtb.Text.Length;
      }

      a_rtb.Select(start, end - start);
    }


    static public void RTBDeleteSelected(RichTextBox a_rtb)
    {
      bool oldReadOnly = a_rtb.ReadOnly;
      a_rtb.ReadOnly = false;

      a_rtb.SelectedText = "";

      a_rtb.ReadOnly = oldReadOnly;
    }


    static public void RTBAppendLine(RichTextBox a_rtb, String a_text)
    {
      bool oldReadOnly = a_rtb.ReadOnly;
      a_rtb.ReadOnly = false;

      a_rtb.AppendText(a_text);
      a_rtb.AppendText( "\r\n" );

      a_rtb.ReadOnly = oldReadOnly;
    }


    static public void RTBAppendLine(RichTextBox a_rtb, String a_text, Color a_color)
    {
      bool oldReadOnly = a_rtb.ReadOnly;
      a_rtb.ReadOnly = false;

      a_rtb.AppendText(a_text);

      int lastLine = a_rtb.Lines.Length - 1;

      RTBSelectLine(a_rtb, lastLine);
      a_rtb.SelectionColor = a_color;

      a_rtb.AppendText( "\r\n" );

      a_rtb.ReadOnly = oldReadOnly;
    }


    static public void RTBDeleteFirstLine(RichTextBox a_rtb)
    {
      if( a_rtb.Lines.Length > 0 )
      {
        RTBSelectLine(a_rtb, 0);
        RTBDeleteSelected(a_rtb);
      }
    }


    static public int RTBGetNumLines(RichTextBox a_rtb)
    {
      return a_rtb.Lines.Length;
    }


    static public void RTBScrollToEnd(RichTextBox a_rtb)
    {
      int lastChar = a_rtb.TextLength - 1;
      if( lastChar < 0 )
      {
        lastChar = 0;
      }
      a_rtb.SelectionStart = lastChar;
      a_rtb.SelectionLength  = 0;
      a_rtb.ScrollToCaret();  
    }

  }
}
