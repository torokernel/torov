//
// GDBstub.pas
//
// This unit contains a gdbstub to debug applications compiled for ToroV.
//
// Copyright (c) 2021 Matias Vara <matiasevara@torokernel.io>
// All Rights Reserved
//
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

unit Gdbstub;


interface

uses Kvm, Sockets;

procedure BPHandler(vcpu: PVCPU);
procedure StepHandler(vcpu: PVCPU);
function GdbstubInit(localport: LongInt; vcpu: PVCPU): Boolean;

implementation
{$MACRO ON}
{$DEFINE XChar := Char}

const
  QSupported : PChar = 'PacketSize=1000';
  Empty : Pchar = '';
  OK : PChar = 'OK';
  Signal05 : PChar = 'S05';
  NB_GENERAL_REG = 16;
  NB_EXTRA_REG = 2;
  MAX_NR_BREAKPOINTS = 50;
  MAX_ADDR_MEM = $400000;
  HEX_CHAR: array[0..15] of XChar = ('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');

var
  ClientSocket: Longint;
  ListenSocket: Longint;
  ServerAddr: TInetSockAddr;
  ClientAddr: TInetSockAddr;
  ClientAddrSize: LongInt;

function DbgSerialGetC: XChar;
var
  r: XChar;
begin
  fprecv(ClientSocket, @r, 1, 0);
  Result := r;
end;

function HexStrtoQWord(start, last: PChar): QWord;
var
  bt: Byte;
  i: PChar;
  Base: QWord;
begin
  i := start;
  Base := 0;
  while (i <> last) do
  begin
    bt := Byte(i^);
    Inc(i);
    if (bt >= Byte('0')) and (bt <= Byte('9')) then
      bt := bt - Byte('0')
    else if (bt >= Byte('a')) and (bt <= Byte('f')) then
      bt := bt - Byte('a') + 10
    else if (bt >= Byte('A')) and (bt <= Byte('F')) then
      bt := bt - Byte('A') + 10; 
    Base := (Base shl 4) or (bt and $F);
  end;
  Result := Base;
end;

function DbgSerialPutC(C: XChar): XChar;
begin
  fpsend(ClientSocket,@C,1,0);
  Result := C;
end;

function DbgWrite(C: PChar; Len: LongInt): LongInt;
Begin
  if Len > 0 then
    fpsend(ClientSocket,C,Len,0);
  Result := 0;
end;

function DbgRead(C: Pchar; BufLen: LongInt; Len: Longint): LongInt;
begin
  Result := 0;
  if BufLen < Len then
    Exit;
  Result := Len;
  while Len > 0 do
  begin
    C^ := DbgSerialGetC;
    Inc(C);
    Dec(Len);
  end; 
end;

function DbgChecksum(buf: PChar; len: LongInt): Byte;
var
  csum: Byte;
begin
  csum := 0;
  while len > 0 do
  begin
    Inc(csum, Byte(buf^));
    Inc(buf);
    Dec(len);
  end;
  Result := csum;
end;

function DbgGetDigit(val: LongInt): Char;
begin
  if (val >= 0) and (val <= $f) then
    Result := HEX_CHAR[val]
  else
    Result := Char(-1);
end;

function DbgEncHex(buff: PChar; buf_len: LongInt; data: PChar; data_len: LongInt): LongInt;
var
  pos: LongInt;
begin
  Result := -1;

  if buf_len < data_len * 2 Then
    Exit;
 
  for pos := 0 to data_len - 1 do
  begin
    buff^ := DbgGetDigit((Byte(data[pos]) shr 4) and $f);
    Inc(buff);
    buff^ := DbgGetDigit(Byte(data[pos]) and $f);
    Inc(buff);
  end;
  Result := data_len *2;
end;

function DbgRecvAck: LongInt;
var
  response: Char;
begin
  response := DbgSerialGetC;
  if response = '+' then
  begin
    Result := 0;
  end else if response = '-' then
  begin
    Result := 1;
  end else
  begin
    Result := -1;
  end;
end;

function DbgSendPacket(PktData: PChar; PktLen: Longint): LongInt;
var
  buf: array[0..2] of Char;
  csum: Char;
begin
  // send packet start
  DbgSerialPutC('$');
  // send packet
  DbgWrite(PktData, Pktlen);
  // send checksum
  buf[0] := '#';
  csum := Char(DbgChecksum(PktData, PktLen));
  DbgEncHex(Pointer(@buf)+1, sizeof(buf)-1, @csum, 1);
  DbgWrite(buf, sizeof(buf));
  Result := DbgRecvAck;
end;

function DbgSendSignalPacket(buf: PChar; buf_len: LongInt; signal: Char): LongInt;
var
  size: LongInt;
  status: LongInt;
begin
  Result := -1;
  if buf_len < 4 Then
    Exit;
  buf[0] := 'S';
  status := DbgEncHex(@buf[1], buf_len-1, @signal, 1);
  if status = -1 Then
    Exit;
  size := 1 + status;
  Result := DbgSendPacket(buf, size);
end;

procedure DbgSendOKPacket;
begin
  DbgSendPacket('OK', 2);
end;

function DbgGetVal (digit: Byte; base: LongInt): Byte;
var
  value: Byte;
begin
  if (digit >= Byte('0')) and (digit <= Byte('9')) then
    value := digit - Byte('0')
  else if (digit >= Byte('a')) and (digit <= Byte('f')) then
    value := digit - Byte('a') + $a
  else if (digit >= Byte('A')) and (digit <= Byte('F')) then
    value := digit - Byte('A') + $a;
  Result := value;
end;

function DbgDecHex(buf: PChar; buf_len: LongInt; data:PChar; data_len: LongInt): LongInt;
var
  pos: LongInt;
  tmp: Byte;
begin
  if buf_len <> data_len*2 then
    Exit;
  for pos := 0 to data_len - 1 do
  begin
    tmp := DbgGetVal(Byte(buf^), 16);
    Inc(buf);
    data[pos] := Char(tmp shl 4);
    tmp := DbgGetVal(Byte(buf^), 16);
    Inc(buf);                  
    data[pos] := Char(Byte(data[pos]) or tmp);
  end;
  Result := 0
end;

procedure DbgRecvPacket(PktBuf: PChar; PktBufLen: LongInt; out PktLen: LongInt);
var
  data: Char;
  expected_csum, actual_csum: Byte;
  buf: array[0..1] of Char;
begin
  actual_csum := 0;
  while true do
  begin
    data := DbgSerialGetC;
    if data = '$' then
      break;
  end;
  PktLen := 0;
  while true do
  begin
    data := DbgSerialGetC;
    if data = '#' then
      break
    else 
    begin
      PktBuf[PktLen] := data;
      Inc(PktLen); 
    end;
    if PktLen > PktBufLen then
      WriteLn('Gdbstub: buffer has been overwritten');
  end;  
  DbgRead(buf, sizeof(buf), 2);
  DbgDecHex(buf, 2, @expected_csum, 1);
  actual_csum := DbgChecksum(PktBuf, PktLen);
  if actual_csum <> expected_csum then
  begin
    DbgSerialPutC('-');
    Exit;
  end;
  DbgSerialPutC('+');
end;

function strlen(C: PChar) : LongInt;
var r: LongInt;
begin
  r := 0;
  while C^ <> #0 do
  begin
    Inc(C);
    Inc(r);
  end;
  Result := r;
end;

function strcomp(P1, P2: PChar): Boolean;
begin
  Result := False;
  while (P1^ = P2^) and (P2^ <> #0) do
  begin
    Inc(P1);
    Inc(P2);
  end;
  if P2^ = #0 then
    Result := True;
end;

var
  breaks: array[0..MAX_NR_BREAKPOINTS-1] of QWord;
  breaksData: array[0..MAX_NR_BREAKPOINTS-1] of Byte;
  count : Byte = 0 ;
  // buff should be per core
  buf: array[0..300] of Char;

procedure DbgHandler (Signal: Boolean; Nr: LongInt; vcpu: PVCPU);
var
  l: array[0..100] of Char;
  Len, i, Size: LongInt;
  reg: QWORD;
  g: ^Char;
  addr: ^Byte;
  p: QWORD;
  preg: ^QWORD;
  mem: PChar;
  regs: kvm_regs;
begin
  GetRegisters(vcpu, @regs);
  preg := @regs;
  mem := vcpu^.vm^.mem;
  if Signal then
    DbgSendSignalPacket(@buf, sizeof(buf), Char(5)); 
  while true do
  begin
    DbgRecvPacket(@buf, sizeof(buf), Len);
    case buf[0] of
      'P': begin
             reg := Byte(HexStrtoQWord(@buf[1], @buf[Len]));
             i := 1;
             while buf[i] <> '=' do
               Inc(i);
             reg := Byte(HexStrtoQWord(@buf[1], @buf[i]));
             DbgDecHex(@buf[i+1], Len - i - 1, Pchar(@p), sizeof(QWORD));
             if reg < 16 then
               preg[reg] := p;
             DbgSendPacket(OK, strlen(OK));
           end;
      'p':begin
            reg := Byte(HexStrtoQWord(@buf[1], @buf[Len]));
            if reg > NB_GENERAL_REG + NB_EXTRA_REG -1 then reg := NB_GENERAL_REG + NB_EXTRA_REG -1;
            DbgEncHex(@buf[0], sizeof(buf), Pchar(@preg[reg]), sizeof(QWORD));
            DbgSendPacket(@buf[0], 8 * 2);
          end;
      'm':begin
            i := 1;
            while buf[i] <> ',' do
              Inc(i);
            reg := HexStrtoQWord(@buf[1], @buf[i]);
            Inc (i);
            Size := HexStrtoQWord(@buf[i], @buf[Len]);
            if Size > sizeof(l) then
              Size := sizeof(l);
            g := Pointer(mem + reg);
            for i:= 0 to Size - 1 do
            begin
              if g > Pointer(MAX_ADDR_MEM) then
              begin
                l[i] := Char(0);
                break;
              end;
              l[i] := g^;
              Inc(g);
            end;  
            DbgEncHex(@buf, sizeof(buf), @l, Size);
            DbgSendPacket(@buf, Size * 2);
          end;
      'g': begin
             // these are only general registers
             DbgEncHex(@buf, sizeof(buf), Pointer(preg), sizeof(regs)-NB_EXTRA_REG * sizeof(QWORD));
             DbgSendPacket(@buf, (sizeof(regs)-NB_EXTRA_REG * sizeof(QWORD)) * 2);
           end;
      'q': begin
             if strcomp(@buf[1], 'Supported') then
             begin
               DbgSendPacket(QSupported, strlen(QSupported));
             end
             else if strcomp(@buf[1], 'fThreadInfo') then
             begin
               DbgSendPacket('l', 1);
             end
             else if buf[1] = 'C' then
             begin
               DbgSendPacket(Empty, strlen(Empty));
             end
             else if strcomp(@buf[1], 'Attached') then
             begin
               DbgSendPacket('1', 1);
             end
             else if strcomp(@buf[1], 'TStatus') then
             begin
                DbgSendPacket(Empty, strlen(Empty));
             end
             else if strcomp(@buf[1], 'Symbol') then
             begin
               DbgSendPacket(OK, strlen(OK)); 
             end
             else if strcomp(@buf[1], 'Offsets') then
             begin
               DbgSendPacket(Empty, strlen(Empty)); 
             end;
             // TODO: handle the case that we do not know the command
           end;
      'z': begin
             i := 3;
             while buf[i] <> ',' do
               Inc(i);
             addr := Pointer(mem + HexStrtoQWord(@buf[3], @buf[i]));
             for i := 0 to MAX_NR_BREAKPOINTS-1 do
             begin
               if breaks[i] = QWORD(addr) then
                 break;
             end;
             addr^ := breaksData[i];
             DbgSendPacket(OK, strlen(OK)); 
          end;
      'Z': begin
             i := 3;
             while buf[i] <> ',' do
               Inc(i);
             addr := Pointer(mem + HexStrtoQWord(@buf[3], @buf[i]));
             for i := 0 to MAX_NR_BREAKPOINTS-1 do
             begin
               if breaks[i] = QWORD(addr) then
                 break;
             end;
             if breaks[i] <> QWORD(addr) then
             begin
               i := count ;
               inc(count) ;
               breaks[i] := QWORD(addr);
               breaksData[i] := addr^;
             end;
             addr^ := $cc;
             DbgSendPacket(OK, strlen(OK));
           end;
      'v': begin
           if strcomp(@buf[1], 'MustReplyEmpty') then
           begin
             DbgSendPacket(Empty, strlen(Empty));   
           end else if strcomp(@buf[1], 'Cont') then
           begin
             DbgSendPacket(Empty, strlen(Empty));
           end;
          end;
      'H': begin
             if strcomp(@buf[1],'g0') then
             begin
               DbgSendPacket(OK, strlen(OK)); 
             end else if buf[1] = 'c' then
             begin
               DbgSendPacket(OK, strlen(OK));
             end;
           end;
      '?': begin
             DbgSendPacket(Signal05, strlen(Signal05));
           end;
      'c': begin
             ClearDebugGuest(vcpu);
             SetupDebugGuest(vcpu);
             if Nr=3 then
               Inc(regs.rip);
             ConfigureRegs(vcpu, @regs);
             break;
           end;
      's': begin
             ClearDebugGuest(vcpu);
             SetDebugGuestStep(vcpu);
             ConfigureRegs(vcpu, @regs);
             break;
           end;
      'D': begin
             DbgSendPacket(OK, strlen(OK));
             break;
           end;
    end;
  end;
end;

procedure BPHandler(vcpu: PVCPU);
begin
  DbgHandler(true, 3, vcpu);
end;

procedure StepHandler(vcpu: PVCPU);
begin
  DbgHandler(true, 1, vcpu);
end;

const
  SOCKET_ERROR = -1;

function GdbstubInit(localport: LongInt; vcpu: PVCPU):Boolean;
var
  i: LongInt;
begin
  Result := false;
  for i := 0 to MAX_NR_BREAKPOINTS-1 do
     breaks[i] := 0 ;
  ListenSocket := fpSocket (AF_INET,SOCK_STREAM,0);
  If ListenSocket = SOCKET_ERROR Then
    Exit;
  ServerAddr.sin_family := AF_INET;
  ServerAddr.sin_port := htons(localport);
  ServerAddr.sin_addr.s_addr := htonl($7F000001);
  If fpBind(ListenSocket,@ServerAddr,sizeof(ServerAddr)) = SOCKET_ERROR Then
    Exit;
  If fpListen (ListenSocket,1) = SOCKET_ERROR Then
    Exit;
  ClientAddrSize := sizeof(ClientAddr);
  WriteLn('Gdbserver is waiting at port: ', localport);
  ClientSocket := fpaccept(ListenSocket,@ClientAddr,@ClientAddrSize);
  If ClientSocket = SOCKET_ERROR Then
    Exit;
  DbgHandler(false, 3, vcpu);
  Result := True;
end;

finalization
  CloseSocket(ClientSocket);
  CloseSocket(ListenSocket);
end.
