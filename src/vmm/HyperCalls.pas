// HyperCalls.pas
//
// This unit handles the VMExits and provides a POSIX interface to the guest.
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
unit HyperCalls;


interface

uses Kvm, BaseUnix, Sockets, sysutils;

const
  MAX_NR_HYPER = 500;
  syscall_nr_ioctl = 16;
  syscall_nr_read  = 0;
  syscall_nr_open  = 2;
  syscall_nr_write = 1;
  syscall_nr_close = 3;
  syscall_nr_getrlimit = 97;
  syscall_nr_mmap = 9;
  syscall_nr_unmmap = 11;
  syscall_nr_socket = 41;
  syscall_nr_bind = 49;
  syscall_nr_listen = 50;
  syscall_nr_accept = 43;
  syscall_nr_sendto = 44;
  syscall_nr_recvfrom = 45;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region, heap: pkvm_user_memory_region): LongInt;
procedure EnableHypercall(nr: LongInt);

implementation

type
  THypercallFunc = function(reg: pkvmregs; region: pkvm_user_memory_region) : LongInt;

var
  HyperCallsAr: array[0..MAX_NR_HYPER-1] of THyperCallFunc;
  HyperCallsAllowed: array[0..MAX_NR_HYPER-1] of Boolean;

function HyperCallIgnore(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  WriteLn('HyperCall ignored!');
end;

procedure EnableHypercall(nr: LongInt);
begin
  HyperCallsAllowed[nr] := True;
end;

// TODO: add a function to convert pointer from guest to user-space
function HyperCallIOCtl(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpIOCtl(regs^.rdi, regs^.rsi, Pointer(region^.userspace_addr + regs^.rdx - region^.guest_phys_addr));
end;

function HyperCallGetRLimit(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := FpGetRLimit(regs^.rdi, Pointer(region^.userspace_addr + regs^.rsi - region^.guest_phys_addr));
end;

function HyperCallWriteConsole(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
var
  tmp: PChar;
begin
  tmp := PChar(region^.userspace_addr + regs^.rcx - region^.guest_phys_addr);
  writeln(tmp);
end;

function HyperCallOpen(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpOpen(PChar(region^.userspace_addr - region^.guest_phys_addr + regs^.rdi), regs^.rsi);
end;

function HyperCallWrite(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpWrite(regs^.rdi, PChar(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx);
end;

function HyperCallRead(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpRead(regs^.rdi, PChar(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx);
end;

function HyperCallClose(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpClose(regs^.rdi);
end;

var
  count: LongInt = 0;

function HyperCallMMap(regs: pkvmregs; heap: pkvm_user_memory_region): LongInt;
begin
  Result := count + heap^.guest_phys_addr;
  count += regs^.rsi;
end;

function HyperCallUNMMap(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := 0;
end;

function HyperCallSocket(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpSocket(regs^.rdi, regs^.rsi, regs^.rdx);
end;

function HyperCallBind(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpBind(regs^.rdi, psockaddr(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx);
end;

function HyperCallListen(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpListen(regs^.rdi, regs^.rsi);
end;

function HyperCallAccept(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpAccept(regs^.rdi, psockaddr(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), pSockLen(regs^.rdx + region^.userspace_addr - region^.guest_phys_addr));
end;

function HyperCallSendTo(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpSendTo(regs^.rdi, psockaddr(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx, regs^.r10, psockaddr(regs^.r8 - region^.userspace_addr - region^.guest_phys_addr), regs^.r9);
end;

function HyperCallRecvFrom(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpRecvFrom(regs^.rdi, Pointer(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx, regs^.r10, psockaddr(regs^.r8 - region^.userspace_addr - region^.guest_phys_addr), pSockLen(regs^.r9 + region^.userspace_addr - region^.guest_phys_addr));
end;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region, heap: pkvm_user_memory_region): LongInt;
begin
  Result := -1;
  // WriteLn('HyperCall: ', nr, ', rip: 0x', IntToHex(regs^.rip, 4));
  if not HyperCallsAllowed[nr] then
  begin
    WriteLn('HyperCall:' , nr, ' has been blocked');
    Result := -1;
    Exit;
  end;

  if nr < MAX_NR_HYPER then
  begin
    if nr = syscall_nr_mmap then
      Result := HyperCallMMap(regs, heap)
    else
      Result := HyperCallsAr[nr](regs, region);
  end;
end;

var
  tmp: LongInt;

initialization
  for tmp:= 0 to MAX_NR_HYPER-1 do
  begin
    HyperCallsAllowed[tmp] := False;
    HyperCallsAr[tmp] := @HyperCallIgnore;
  end;
  HyperCallsAr[syscall_nr_close] := @HyperCallClose;
  HyperCallsAr[syscall_nr_read] := @HyperCallRead;
  HyperCallsAr[syscall_nr_open] := @HyperCallOpen;
  HyperCallsAr[syscall_nr_write] := @HyperCallWrite;
  HyperCallsAr[syscall_nr_ioctl] := @HyperCallIOCtl;
  HyperCallsAr[syscall_nr_getrlimit] := @HyperCallGetRLimit;
  HyperCallsAr[syscall_nr_mmap] := @HyperCallMMap;
  HyperCallsAr[syscall_nr_unmmap] := @HyperCallUNMMap;
  HyperCallsAr[syscall_nr_socket] := @HyperCallSocket;
  HyperCallsAr[syscall_nr_bind] := @HyperCallBind;
  HyperCallsAr[syscall_nr_listen] := @HyperCallListen;
  HyperCallsAr[syscall_nr_accept] := @HyperCallAccept;
  HyperCallsAr[syscall_nr_sendto] := @HyperCallSendTo;
  HyperCallsAr[syscall_nr_recvfrom] := @HyperCallRecvFrom;
  HyperCallsAr[499] := @HyperCallWriteConsole;
end.
