//
// This unit handles the VMExits and provides a POSIX interface to the guest.
//
// Copyright (c) 2021 Matias Vara <matiasevara@gmail.com>
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

uses Kvm, BaseUnix;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region, heap: pkvm_user_memory_region): LongInt;

implementation

const
  MAX_NR_HYPER = 500;
  syscall_nr_ioctl = 16;
  syscall_nr_read  = 0;
  syscall_nr_write = 1;
  syscall_nr_getrlimit = 97;
  syscall_nr_mmap = 9;

type
  THypercallFunc = function(reg: pkvmregs; region: pkvm_user_memory_region) : LongInt;

var
  HyperCallsAr: array[0..MAX_NR_HYPER-1] of THyperCallFunc;

function HyperCallIgnore(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  WriteLn('HyperCall ignored!');
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

function HyperCallWrite(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpWrite(regs^.rdi, PChar(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx);
end;

function HyperCallRead(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := fpRead(regs^.rdi, PChar(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx);
end;

var
  count: LongInt = 0;

function HyperCallMMap(regs: pkvmregs; heap: pkvm_user_memory_region): LongInt;
begin
  Result := count + heap^.guest_phys_addr;
  count += regs^.rsi; 
end;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region, heap: pkvm_user_memory_region): LongInt;
begin
  Result := -1;
  WriteLn('HyperCall:', nr);
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
    HyperCallsAr[tmp] := @HyperCallIgnore;
  end;
  HyperCallsAr[syscall_nr_read] := @HyperCallRead;
  HyperCallsAr[syscall_nr_write] := @HyperCallWrite;
  HyperCallsAr[syscall_nr_ioctl] := @HyperCallIOCtl;
  HyperCallsAr[syscall_nr_getrlimit] := @HyperCallGetRLimit;
  HyperCallsAr[syscall_nr_mmap] := @HyperCallMMap;
  HyperCallsAr[499] := @HyperCallWriteConsole;
end.
