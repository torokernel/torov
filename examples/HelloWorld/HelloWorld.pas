{$ASMMODE INTEL}
{$mode delphi} 

uses ToroVSys;

function DoHyperCall(nr: DWORD; arg1, arg2, arg3: QWORD): QWORD; cdecl;assembler;
asm
  mov eax, nr
  mov rbx, arg1
  mov rcx, arg2
  mov rdx, arg3
  out $10, eax
end;

procedure WriteConsole(s: PChar);
begin
  DoHyperCall(1, 0, PtrUInt(s),0);
end;

begin
WriteConsole('Hello World!'); 
WriteConsole('I am ToroV!'); 

asm
hlt
end;
end.
