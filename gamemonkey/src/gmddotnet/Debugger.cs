using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.IO;

namespace gmDebuggerNet
{

  // Network message packet codes
  public class FOURCC
  {
    public static Int32 MakeFOURCC(char ch0, char ch1, char ch2, char ch3)
    {
      return ((Int32)(byte)(ch0) | ((byte)(ch1) << 8) | ((byte)(ch2) << 16) | ((byte)(ch3) << 24));
    }

    public static readonly int ID_mrun = FOURCC.MakeFOURCC('m','r','u','n');
    public static readonly int ID_msin = FOURCC.MakeFOURCC('m','s','i','n');
    public static readonly int ID_msou = FOURCC.MakeFOURCC('m','s','o','u');
    public static readonly int ID_msov = FOURCC.MakeFOURCC('m','s','o','v');
    public static readonly int ID_mgct = FOURCC.MakeFOURCC('m','g','c','t');
    public static readonly int ID_mgsr = FOURCC.MakeFOURCC('m','g','s','r');
    public static readonly int ID_mgsi = FOURCC.MakeFOURCC('m','g','s','i');
    public static readonly int ID_mgti = FOURCC.MakeFOURCC('m','g','t','i');
    public static readonly int ID_mgvi = FOURCC.MakeFOURCC('m','g','v','i');
    public static readonly int ID_msbp = FOURCC.MakeFOURCC('m','s','b','p');
    public static readonly int ID_mbrk = FOURCC.MakeFOURCC('m','b','r','k');
    public static readonly int ID_mend = FOURCC.MakeFOURCC('m','e','n','d');

    public static readonly int ID_dbrk = FOURCC.MakeFOURCC('d','b','r','k');
    public static readonly int ID_dexc = FOURCC.MakeFOURCC('d','e','x','c');
    public static readonly int ID_drun = FOURCC.MakeFOURCC('d','r','u','n');
    public static readonly int ID_dstp = FOURCC.MakeFOURCC('d','s','t','p');
    public static readonly int ID_dsrc = FOURCC.MakeFOURCC('d','s','r','c');
    public static readonly int ID_dctx = FOURCC.MakeFOURCC('d','c','t','x');
    public static readonly int ID_call = FOURCC.MakeFOURCC('c','a','l','l');
    public static readonly int ID_vari = FOURCC.MakeFOURCC('v','a','r','i');
    public static readonly int ID_done = FOURCC.MakeFOURCC('d','o','n','e');
    public static readonly int ID_dsri = FOURCC.MakeFOURCC('d','s','r','i');
    public static readonly int ID_srci = FOURCC.MakeFOURCC('s','r','c','i');
    public static readonly int ID_dthi = FOURCC.MakeFOURCC('d','t','h','i');
    public static readonly int ID_thri = FOURCC.MakeFOURCC('t','h','r','i');
    public static readonly int ID_derr = FOURCC.MakeFOURCC('d','e','r','r');
    public static readonly int ID_dmsg = FOURCC.MakeFOURCC('d','m','s','g');
    public static readonly int ID_dack = FOURCC.MakeFOURCC('d','a','c','k');
    public static readonly int ID_dend = FOURCC.MakeFOURCC('d','e','n','d');
  }


  // Debug session for message handling
  public class DebuggerSession
  {
    protected Network m_network;                            // Network connection
    protected MemoryStream m_outStream;                     // Current send stream
    protected MemoryStream m_inStreamCur;                   // Current rec stream
    protected System.Text.UTF8Encoding m_textEncoding;      // helper for string to byte array conversion

    // Constructor
    public DebuggerSession(Network a_network)
    {
      m_network = a_network;

      m_textEncoding = new System.Text.UTF8Encoding();

      m_outStream = new MemoryStream();
      m_inStreamCur = null;
    }

    // Process incomming net messages
    public void Update(DebuggerRecInterface a_iRec)
    {
      int msgLimit = 100; // NOTE: This can improve UI responsiveness, but can cause the queue buffer to grow!
      for(;;) // While messages are queued
      {
        m_inStreamCur = m_network.GetFirstInQueue();
        
        // Limit messages in case we cause app to freeze
        if( --msgLimit <= 0 )
        {
          break;
        }

        if (m_inStreamCur == null)
        {
          break;
        }

        int msgId = 0;
        Unpack(out msgId);

        if( FOURCC.ID_dbrk == msgId )
        {
          int threadId, sourceId, lineNum;

          Unpack(out threadId).Unpack(out sourceId).Unpack(out lineNum);
          a_iRec.gmDebuggerBreak(threadId, sourceId, lineNum);
        }
        else if( FOURCC.ID_drun == msgId )
        {
          int threadId;

          Unpack(out threadId);
          a_iRec.gmDebuggerRun(threadId);
        }
        else if( FOURCC.ID_dstp == msgId )
        {
          int threadId;

          Unpack(out threadId);
          a_iRec.gmDebuggerStop(threadId);
        }
        else if( FOURCC.ID_dsrc == msgId )
        {
          int sourceId;
          String sourceName, source;

          Unpack(out sourceId).Unpack(out sourceName).Unpack(out source);
          a_iRec.gmDebuggerSource(sourceId, sourceName, source);
        }
        else if( FOURCC.ID_dexc == msgId )
        {
          int threadId;

          Unpack(out threadId);
          a_iRec.gmDebuggerException(threadId);
        }
        else if( FOURCC.ID_dctx == msgId )
        {
          int threadId, callFrame;

          Unpack(out threadId).Unpack(out callFrame); // thread id, callframe
          a_iRec.gmDebuggerBeginContext(threadId, callFrame);
          for(;;)
          {
            int id;
            Unpack(out id);
            if(id == FOURCC.ID_call)
            {
              String functionName, thisSymbol, thisValue;
              int sourceId, lineNum, thisId;

              Unpack(out callFrame).Unpack(out functionName).Unpack(out sourceId).Unpack(out lineNum).Unpack(out thisSymbol).Unpack(out thisValue).Unpack(out thisId);
              a_iRec.gmDebuggerContextCallFrame(callFrame, functionName, sourceId, lineNum, thisSymbol, thisValue, thisId);
            }
            else if (id == FOURCC.ID_vari)
            {
              int varId;
              String varSymbol, varValue;

              Unpack(out varSymbol).Unpack(out varValue).Unpack(out varId);
              a_iRec.gmDebuggerContextVariable(varSymbol, varValue, varId);
            }
            else if (id == FOURCC.ID_done)
            {
              break;
            }
            else
            {
              break;
            }
          }
          a_iRec.gmDebuggerEndContext();
       }
       else if( FOURCC.ID_dsri == msgId )
       {
          // todo
          break;
        }
        else if( FOURCC.ID_dthi == msgId )
        {
          a_iRec.gmDebuggerBeginThreadInfo();
          for(;;)
          {
            int id, threadId, threadState;

            Unpack(out id);
            if(id == FOURCC.ID_thri)
            {
              Unpack(out threadId).Unpack(out threadState);
              a_iRec.gmDebuggerThreadInfo(threadId, threadState);
            }
            else if (id == FOURCC.ID_done)
            {
              break;
            }
            else
            {
              break;
            }
          }
          a_iRec.gmDebuggerEndThreadInfo();
        }
        else if( FOURCC.ID_derr == msgId )
        {
          String errorMessage;

          Unpack(out errorMessage);
          a_iRec.gmDebuggerError(errorMessage);
        }
        else if( FOURCC.ID_dmsg == msgId )
        {
          String message;

          Unpack(out message);
          a_iRec.gmDebuggerMessage(message);
        }
        else if( FOURCC.ID_dack == msgId )
        {
          int response, posNeg;

          Unpack(out response).Unpack(out posNeg);
          a_iRec.gmDebuggerAck(response, posNeg);
        }
        else if( FOURCC.ID_dend == msgId )
        {
          a_iRec.gmDebuggerQuit();
        }
        else
        {
          // unknown Id
        }
      }
    }

    public DebuggerSession Pack(int a_value)
    {
      m_outStream.Write(BitConverter.GetBytes(a_value), 0, 4);
      return this;
    }

    public DebuggerSession Pack(String a_string)
    {
      m_outStream.Write(m_textEncoding.GetBytes(a_string), 0, a_string.Length);
      return this;
    }

    // TODO, something like this is needed for 64bit
    //public DebuggerSession UnpackIntPtr(out long a_value)
    //{
    //  if( Is64bitGM )
    //  {
    //    byte[] value = new byte[8];

    //    m_inStreamCur.Read(value, 0, 8);
    //    a_value = BitConverter.ToInt64(value, 0);
    //    return this;
    //  }
    //  else
    //  {
    //    byte[] value = new byte[4];

    //    m_inStreamCur.Read(value, 0, 4);
    //    a_value = BitConverter.ToInt32(value, 0);
    //    return this;
    //  }
    //}

    public DebuggerSession Unpack(out int a_value)
    {
      byte[] value = new byte[4];

      m_inStreamCur.Read(value, 0, 4);
      a_value = BitConverter.ToInt32(value, 0);
      return this;
    }

    public DebuggerSession Unpack(out String a_string)
    {
      a_string = "";

      byte[] rawBuffer = m_inStreamCur.GetBuffer();

      long startPos = m_inStreamCur.Position;
      long curPos = startPos;
      long endPos = m_inStreamCur.Length - 1;
      for (; curPos <= endPos; ++curPos)
      {
        if (rawBuffer[curPos] == 0)
        {
          break;
        }
      }

      long stringLength = curPos - startPos;
      if (stringLength > 0)
      {
        a_string = m_textEncoding.GetString(rawBuffer, (int)startPos, (int)stringLength);
      }

      m_inStreamCur.Seek(startPos + stringLength + 1, SeekOrigin.Begin); // Skip past string we just read

      return this;
    }

    public void Send()
    {
      m_network.Send(m_outStream);
      m_outStream.SetLength(0); // Reset buffer to recycle
    }

    public void gmMachineRun(int a_threadId)
    {
      Pack(FOURCC.ID_mrun).Pack(a_threadId).Send();
    }

    public void gmMachineStepInto(int a_threadId)
    {
      Pack(FOURCC.ID_msin).Pack(a_threadId).Send();
    }

    public void gmMachineStepOver(int a_threadId)
    {
      Pack(FOURCC.ID_msov).Pack(a_threadId).Send();
    }

    public void gmMachineStepOut(int a_threadId)
    {
      Pack(FOURCC.ID_msou).Pack(a_threadId).Send();
    }

    public void gmMachineGetContext(int a_threadId, int a_callframe)
    {
      Pack(FOURCC.ID_mgct).Pack(a_threadId).Pack(a_callframe).Send();
    }

    public void gmMachineGetSource(int a_sourceId)
    {
      Pack(FOURCC.ID_mgsr).Pack(a_sourceId).Send();
    }

    public void gmMachineGetSourceInfo()
    {
      Pack(FOURCC.ID_mgsi).Send();
    }

    public void gmMachineGetThreadInfo()
    {
      Pack(FOURCC.ID_mgti).Send();
    }

    public void gmMachineGetVariableInfo(int a_variableId)
    {
      Pack(FOURCC.ID_mgvi).Pack(a_variableId).Send();
    }

    public void gmMachineSetBreakPoint(int a_responseId, int a_sourceId, int a_lineNumber, int a_threadId, int a_enabled)
    {
      Pack(FOURCC.ID_msbp).Pack(a_responseId).Pack(a_sourceId).Pack(a_lineNumber).Pack(a_threadId).Pack(a_enabled).Send();
    }

    public void gmMachineBreak(int a_threadId)
    {
      Pack(FOURCC.ID_mbrk).Pack(a_threadId).Send();
    }

    public void gmMachineQuit()
    {
      Pack(FOURCC.ID_mend).Send();
    }

  };

  
  // Interface expected for rec messages
  public interface DebuggerRecInterface
  {
    void gmDebuggerBreak(int a_threadId, int a_sourceId, int a_lineNumber);
    void gmDebuggerRun(int a_threadId);
    void gmDebuggerStop(int a_threadId);
    void gmDebuggerSource(int a_sourceId, String a_sourceName, String a_source);
    void gmDebuggerException(int a_threadId);

    void gmDebuggerBeginContext(int a_threadId, int a_callFrame);
    void gmDebuggerContextCallFrame(int a_callFrame, String a_functionName, int a_sourceId, int a_lineNumber, String a_thisSymbol, String a_thisValue, int a_thisId);
    void gmDebuggerContextVariable(String a_varSymbol, String a_varValue, int a_varId);
    void gmDebuggerEndContext();

    void gmDebuggerBeginSourceInfo();
    void gmDebuggerSourceInfo(int a_sourceId, String a_sourceName);
    void gmDebuggerEndSourceInfo();

    void gmDebuggerBeginThreadInfo();
    void gmDebuggerThreadInfo(int a_threadId, int a_threadState);
    void gmDebuggerEndThreadInfo();

    void gmDebuggerError(String a_error);
    void gmDebuggerMessage(String a_message);
    void gmDebuggerAck(int a_response, int a_posNeg);
    void gmDebuggerQuit();
  }

  // State of the remote virtual machine, filled from rec messages
  public class DebuggerState
  {
    // Thread info
    public class ThreadInfo
    {
      public int m_id;
      public int m_state;

      public ThreadInfo()
      {
        m_id = 0;
        m_state = 0;
      }

      public ThreadInfo(int a_id, int a_state)
      {
        m_id = a_id;
        m_state = a_state;
      }

    };

    // Source code info
    public class Source
    {
      public String m_source;
      public uint m_id;

      public Source()
      {
      }

      public Source(String a_source, uint a_id)
      {
        m_source = a_source;
        m_id = a_id;
      }
    };

    // Breakpoint info
    public class gmdBreakPoint
    {
      public bool m_enabled;
      public bool m_allThreads;
      public int m_responseId;
      public int m_lineNumber;
      public int m_threadId;
      public uint m_sourceId;
    };


    public List<ThreadInfo> m_threads;            // current running threads.
    public List<Source> m_sources;                // list of source codes
    public int m_currentCallFrame;                // current stack frame
    public uint m_sourceId;                       // source id of the source code currently being viewed, 0 for none
    public int m_currentDebugThread;              // thread id of the current context thread
    public int m_currentPos;                      // execute pos. -1 for invalid.
    public gmdBreakPoint m_breakPoint;            // last break point command
    public int m_responseId;                      // current rolling response id
    public int m_lineNumberOnSourceRcv;           // current line number in loaded source
    public bool m_isDebugging;

    public DebuggerState()
    {
      Clear();
    }

    public void Clear()
    {
      m_threads = new List<ThreadInfo>();
      m_sources = new List<Source>();
      m_currentDebugThread = 0;
      m_currentPos = -1;
      m_breakPoint = new gmdBreakPoint();
      m_responseId = 100;
      m_sourceId = 0;
      m_lineNumberOnSourceRcv = -1;
      m_isDebugging = false;
    }

  };
}
