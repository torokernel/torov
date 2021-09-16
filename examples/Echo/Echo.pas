Uses Sockets;

Var
  Buffer           : String[255];
  Count            : LongInt;
  ClientSocket     : Longint;
  ListenSocket     : Longint;
  ServerAddr       : TInetSockAddr;
  ClientAddr       : TInetSockAddr;
  ClientAddrSize   : LongInt;

Procedure PrintError (Const Msg : String);
Begin
  Writeln (Msg,SocketError);
  Halt(100);
End;

const
  SOCKET_ERROR = -1;

Begin
  ListenSocket := fpSocket (AF_INET,SOCK_STREAM,0);
  If ListenSocket = SOCKET_ERROR Then
    PrintError ('Server : Socket : ');
  ServerAddr.sin_family := AF_INET;
  { port 50000 in network order }
  ServerAddr.sin_port := htons(50000);
  ServerAddr.sin_addr.s_addr := htonl($7F000001);
  If fpBind(ListenSocket,@ServerAddr,sizeof(ServerAddr)) = SOCKET_ERROR Then
   PrintError ('Server : Bind : ');
  If fpListen (ListenSocket,1) = SOCKET_ERROR Then
   PrintError ('Server : Listen : ');
  Writeln('Waiting for Connect from Client, run now sock_cli in an other tty');
  ClientAddrSize := sizeof(ClientAddr);
  ClientSocket := fpaccept(ListenSocket,@ClientAddr,@ClientAddrSize);
  If ClientSocket = SOCKET_ERROR Then
   PrintError('Server : Accept : ');
  Buffer := 'This is a Server running as ToroV';
  Count := Length(Buffer);
  If (fpsend(ClientSocket,@Buffer[1],Count,0) = Count) Then
    Begin
    Repeat
      Count := fprecv(ClientSocket,@Buffer[1],255,0);
      If (Count <> SOCKET_ERROR) And (Count > 0) Then
        Begin
        SetLength(Buffer,Count);
        Writeln('Server : read : ',Buffer);
        End;
    Until (Count = SOCKET_ERROR) Or (Count = 0);
    End;
  CloseSocket(ClientSocket);
  CloseSocket(ListenSocket);
End.
