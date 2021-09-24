//
// Echo Example
//
// This example echoes on port 50000. This example is based on the example
// presented at https://www.pascalgamedevelopment.com/archive/index.php/t-7404.html
//
// Copyright (c) 2021 Matias Vara <matiasevara@torokernel.io>
// All Rights Reserved
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

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
