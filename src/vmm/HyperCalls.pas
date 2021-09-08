Unit HyperCalls;


interface

uses Kvm;

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region: pkvm_user_memory_region): Boolean;

implementation

Const
  MAX_NR_HYPER = 30;

Type
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

function HyperCallEntry(nr: LongInt; regs: pkvmregs; region: pkvm_user_memory_region): Boolean;
begin
  Result := False;
  WriteLn('HyperCall:', nr);
  if nr < MAX_NR_HYPER then
  begin
    HyperCallsAr[nr](regs, region);
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
  HyperCallsAr[1] := @HyperCallWriteConsole;
end.
