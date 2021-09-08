Unit HyperCalls;


interface

uses Kvm, BaseUnix;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region: pkvm_user_memory_region): Boolean;

implementation

const
  MAX_NR_HYPER = 500;
  syscall_nr_ioctl = 16; // FpIOCtl:=do_SysCall(syscall_nr_ioctl,tsysparam(fd),tsysparam(Request),TSysParam(data));
  syscall_nr_read  = 0;
  syscall_nr_write = 1;

type
  THypercallFunc = function(reg: pkvmregs; region: pkvm_user_memory_region) : LongInt;

var
  HyperCallsAr: array[0..MAX_NR_HYPER-1] of THyperCallFunc;


function HyperCallIgnore(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
begin
  WriteLn('HyperCall ignored!');
end;

function HyperCallWriteConsole(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
var
  tmp: PChar;
begin
  tmp := PChar(region^.userspace_addr + regs^.rcx - region^.guest_phys_addr);
  writeln(tmp);
end;

function HyperCallWrite(regs: pkvmregs; region: pkvm_user_memory_region): LongInt;
var
  tmp: PChar;
begin
  Result := fpWrite(regs^.rdi, PChar(regs^.rsi + region^.userspace_addr - region^.guest_phys_addr), regs^.rdx);
end;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region: pkvm_user_memory_region): Boolean;
begin
  Result := False;
  WriteLn('HyperCall:', nr);
  if nr < MAX_NR_HYPER then
  begin
    HyperCallsAr[nr](regs, region);
    // TODO: set result of hypercall in rax
    Result := True;
  end;
end;

var 
  tmp: LongInt;

initialization
  for tmp:= 0 to MAX_NR_HYPER-1 do
  begin
    HyperCallsAr[tmp] := @HyperCallIgnore;
  end;
  HyperCallsAr[syscall_nr_write] := @HyperCallWrite;
  HyperCallsAr[499] := @HyperCallWriteConsole;
end.
