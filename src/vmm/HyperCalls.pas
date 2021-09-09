Unit HyperCalls;


interface

uses Kvm, BaseUnix;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region: pkvm_user_memory_region): LongInt;

implementation

const
  MAX_NR_HYPER = 500;
  syscall_nr_ioctl = 16;
  syscall_nr_read  = 0;
  syscall_nr_write = 1;
  syscall_nr_getrlimit = 97;

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

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  Result := -1;
  //WriteLn('HyperCall:', nr);
  if nr < MAX_NR_HYPER then
    Result := HyperCallsAr[nr](regs, region);
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
  HyperCallsAr[499] := @HyperCallWriteConsole;
end.
