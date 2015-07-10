using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

using System.Threading;
using System.Net.Sockets;
using System.Net;
using System.Windows.Forms;
using System.IO;

namespace gmDebuggerNet
{
  // A raw buffer to recieve network data
  public class NetBuffer
  {
    public const int BufferSize = 1024;           // Size of buffer
    public byte[] m_bytes = new byte[BufferSize];  // Buffer
  }

  // State used to make logical packets from the TCP stream
  public enum ReadState
  {
    StateSearching = 0,
    StateGettingMessage = 1,
  };

  // Network communication helper, connect, send, receive data via sockets
  public class Network
  {
    // Status when net work is closed
    public enum CloseStatus
    {
      Unknown,
      Closed,                                     // We closed the connection
      LostConnection,                             // Connection was lost or dropped
    };

    public static readonly int PACKET_ID = 0x4fe27d9a;

    protected Socket m_socketListener;                                // Socket to listen for connection
    protected Socket m_socketWorker;                                  // Socket to send and rec data
    protected NetBuffer m_buffer = new NetBuffer();                   // Rec buffer
    public bool IsConnected = false;                                  // Is connected?
    public CloseStatus m_closeStatus = CloseStatus.Unknown;           // Status when closed
    protected MemoryStream m_inStreamTemp;                            // Temporary rec stream
    protected Queue<MemoryStream> m_inQueue;                          // Queue of rec messages
    protected Mutex m_mutexInQueue;                                   // semaphore for rec queue
    protected ReadState m_readState = ReadState.StateSearching;       // State for parsing rec stream into messages
    protected int m_readBytesNeeded = 4 + 4; // Id + size             // Bytes needed for current message (in rec stream)
    
    public Network()
    {
      m_mutexInQueue = new Mutex();

      m_inStreamTemp = new MemoryStream();
      m_inQueue = new Queue< MemoryStream >();
    }

    public void ClearCloseStatus()
    {
      m_closeStatus = CloseStatus.Unknown;
    }

    public MemoryStream GetFirstInQueue()
    {
      MemoryStream ret = null;
      
      m_mutexInQueue.WaitOne();

      if( m_inQueue.Count > 0 )
      {
        ret = m_inQueue.Dequeue();
      } 
      
      m_mutexInQueue.ReleaseMutex();

      return ret;
    }
    
    // Cleanup connection
    public void Done()
    {
      Close();
    }

    // Start new connection
    public bool Open(int a_port)
    {
      m_readState = ReadState.StateSearching;
      m_readBytesNeeded = 4 + 4; // Id + size

      try
      {
        // Try to open socket at port
        m_socketListener = new Socket(AddressFamily.Unspecified, SocketType.Stream, ProtocolType.Tcp);
        // Bind socket to IP
        m_socketListener.Bind(new IPEndPoint(IPAddress.Any, a_port));
        // Start listening
        m_socketListener.Listen(4);
        // Callback for any client connection attempts
        m_socketListener.BeginAccept(new AsyncCallback(CallbackClientConnect), null);
      }
      catch( SocketException ex )
      {
        MessageBox.Show( ex.Message );
        return false;
      }

      return true;
    }

    public void CallbackClientConnect(IAsyncResult a_result)
    {
      try
      {
        m_socketWorker = m_socketListener.EndAccept(a_result);
        m_socketWorker.BeginReceive(m_buffer.m_bytes, 0, NetBuffer.BufferSize, 0, new AsyncCallback(CallbackReceive), this);

        IsConnected = true;
      }
      catch (ObjectDisposedException)
      {
        System.Diagnostics.Debugger.Log(0, "1", "\n CallbackClientConnection: Socket has been closed\n");
      }
      catch (SocketException ex)
      {
        MessageBox.Show(ex.Message);
      }

    }

    public void CallbackReceive(IAsyncResult a_asyncResult)
    {
      if( m_socketWorker == null )
      {
        return; // Connection closed
      }
      if( m_socketWorker.Connected == false )
      {
        return; // Lost connection
      }
      
      bool cleanupNeeded = false;

      try // NOTE: socket can lose connection while this async function is in process, above checks are inadequate alone
      {
        int numBytesRec = m_socketWorker.EndReceive(a_asyncResult);
        int curRecPos = 0;

        if (numBytesRec > 0)
        {
          while( numBytesRec > 0 ) // Consume received bytes
          {
            int have = (numBytesRec > m_readBytesNeeded) ? m_readBytesNeeded : numBytesRec;
            m_readBytesNeeded -= have;
            numBytesRec -= have;

            // Append current logical packet
            m_inStreamTemp.Write(m_buffer.m_bytes, curRecPos, have);
            curRecPos += have;

            // Can we change state?
            if( m_readBytesNeeded == 0 )
            {
              if( m_readState == ReadState.StateSearching )
              {
                m_readState = ReadState.StateGettingMessage;

                // Check first 4 bytes is ID
                int packetId = BitConverter.ToInt32(m_inStreamTemp.GetBuffer(), 0);

                if( packetId != PACKET_ID )
                {
                  throw new Exception("Unexpected Packet Id");
                }
              
                // Size is next 4 bytes
                int packetSize = BitConverter.ToInt32(m_inStreamTemp.GetBuffer(), 4);
              
                m_readBytesNeeded = packetSize;
              }
              else if( m_readState == ReadState.StateGettingMessage )
              {
                m_readState = ReadState.StateSearching;

                // Append complete packet to queue
                m_inStreamTemp.Seek(4 + 4, SeekOrigin.Begin); // Skip past packet Id, Size
                m_mutexInQueue.WaitOne();
                m_inQueue.Enqueue( m_inStreamTemp );
                m_mutexInQueue.ReleaseMutex();
              
                m_inStreamTemp = new MemoryStream();
                m_readBytesNeeded = 4 + 4; // Id + size
              }
            }
          }
        }

        m_socketWorker.BeginReceive(m_buffer.m_bytes, 0, NetBuffer.BufferSize, 0, new AsyncCallback(CallbackReceive), this);
      }
      catch ( System.Net.Sockets.SocketException ) //ex)
      {
        //MessageBox.Show(ex.Message);
        cleanupNeeded = true;
      }
      finally
      {
        if( cleanupNeeded )
        {
          Close();
          m_closeStatus = CloseStatus.LostConnection;
        }
      }
    }

    public void Close()
    {
       if( m_socketListener != null )
      {
        if( m_socketListener.Connected )
        {
          m_socketListener.Shutdown(SocketShutdown.Both);
        }
        m_socketListener.Close();
        m_socketListener = null;
      }
      if( m_socketWorker != null )
      {
        if (m_socketWorker.Connected)
        {
          m_socketWorker.Shutdown(SocketShutdown.Both);
        }
        m_socketWorker.Close();
        m_socketWorker = null;
      }

      IsConnected = false;
    }

    public void Send(MemoryStream a_stream)
    {
      if( IsConnected )
      {
        // Send Syncronously
/*
        // Prefix with Id and Size as these are not part of the buffer
        m_socketWorker.Send(BitConverter.GetBytes((int)PACKET_ID), 4, SocketFlags.None);
        m_socketWorker.Send(BitConverter.GetBytes((int)a_stream.Length), 4, SocketFlags.None);

        m_socketWorker.Send(a_stream.GetBuffer(), (int)a_stream.Length, SocketFlags.None);
 */
        // Send Asyncronously

        // Copy message to temporary buffer so we can fire and forget (May be more efficient to recycle a pool of buffers)
        int outBufferSize = (int)a_stream.Length + 4 + 4;
        int curOffset = 0;
        byte[] outBufferTemp = new byte[ outBufferSize ];
        
        Buffer.BlockCopy(BitConverter.GetBytes((int)PACKET_ID), 0, outBufferTemp, curOffset, 4);
        curOffset += 4;
        Buffer.BlockCopy(BitConverter.GetBytes((int)a_stream.Length), 0, outBufferTemp, curOffset, 4);
        curOffset += 4;
        Buffer.BlockCopy(a_stream.GetBuffer(), 0, outBufferTemp, curOffset, (int)a_stream.Length);

        // Start the async send
        m_socketWorker.BeginSend( outBufferTemp, 0, outBufferSize,0, new AsyncCallback(SendCallback), m_socketWorker);
      }
      a_stream.SetLength(0); // Reset buffer to recycle
    }

    private void SendCallback(IAsyncResult ar)
    {
      try
      {
        // Retrieve the socket from the state object.
        //Network This = (Network)ar.AsyncState;
        //Socket socket = This.m_socketWorker;

        // Complete sending the data to the remote device.
        int bytesSent = m_socketWorker.EndSend(ar);
      }
      catch (Exception )//ex)
      {
        //MessageBox.Show(ex.Message);
      }
    }

  }
}
