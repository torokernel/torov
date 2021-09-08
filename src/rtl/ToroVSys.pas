// ToroVSys:
// 
// This unit contains the RTL to compile with the application
//
Unit ToroVSys;




interface



{$ASMMODE INTEL}
{$mode delphi} 

implementation


function InitSystem(notused: pointer): PtrInt; external name 'PASCALMAIN';

var
  ToroVMemoryManager: TMemoryManager;

function ToroAllocMem(Size: PtrUInt): Pointer;
begin
asm
mov rax, $1234
hlt
end;
  {  Result := ToroGetMem(Size);
  if Result <> nil then
      FillChar(Result^, Size, 0);}
end;

function ToroReAllocMem(var P: Pointer; NewSize: PtrUInt): Pointer;
begin
asm
mov rax, $1234
hlt
end;
end;

function ToroGetMem(Size: PtrUInt): Pointer;
begin
asm
mov rax, $1234
hlt
end;
end;

function ToroFreeMem(P: Pointer): PtrUInt;
begin
asm
mov rax, $1234
hlt
end;

end;
var  
  ToroThreadManager: TThreadManager;

// TODO: critical sections are important when using objpascal in multicore
procedure SysInitCriticalSection(var cs : TRTLCriticalSection);
begin
end;

procedure SysDoneCriticalSection(var cs : TRTLCriticalSection);
begin
end;

procedure SysLeaveCriticalSection(var cs : TRTLCriticalSection);
begin
end;

procedure SysEnterCriticalSection(var cs : TRTLCriticalSection);
begin
end;

const
   THREADVAR_BLOCKSIZE: DWORD = 0;

procedure SysInitThreadVar(var Offset: DWORD; Size: DWORD);
begin
  Offset := THREADVAR_BLOCKSIZE;
  THREADVAR_BLOCKSIZE := THREADVAR_BLOCKSIZE+Size;
end;

var
  tls: array[0..1000] of Byte;

function SysRelocateThreadvar(Offset: DWORD): Pointer;
begin
  {CurrentThread := GetCurrentThread;
  Result := Pointer(PtrUInt(CurrentThread.TLS)+Offset)}
  Result := Pointer(PtrUInt(@tls[0])+Offset);
end;

procedure SysAllocateThreadVars;
var
  CpuID: Byte;
begin
   {  CpuID := GetApicID;
  CPU[CpuID].CurrentThread.TLS := ToroGetMem(THREADVAR_BLOCKSIZE) ;
  Panic(CPU[CpuID].CurrentThread.TLS = nil, 'SysAllocateThreadVars: Out of memory', []);
  }end;

function SysGetCurrentThreadID: TThreadID;
begin
Result := 10;
end;

procedure Init;
begin
  ToroVMemoryManager.GetMem := @ToroGetMem;
  ToroVMemoryManager.FreeMem := @ToroFreeMem;
  ToroVMemoryManager.AllocMem := @ToroAllocMem;
  ToroVMemoryManager.ReAllocMem := @ToroReAllocMem;
  ToroVMemoryManager.RelocateHeap := nil;
  ToroVMemoryManager.InitThread := nil;
  SetMemoryManager(ToroVMemoryManager);
  
  with ToroThreadManager do
  begin
    InitManager            := nil;
    DoneManager            := nil;
    {    BeginThread            := @SysBeginThread;
    EndThread              := @SysEndThread;
    SuspendThread          := @SysSuspendThread;
    ResumeThread           := @SysResumeThread;
    KillThread             := @SysKillthread;
    ThreadSwitch           := @SysThreadSwitch;}
    WaitForThreadTerminate := nil;
    ThreadSetPriority      := nil;
    ThreadGetPriority      := nil;
    GetCurrentThreadId     := @SysGetCurrentThreadID;
    InitCriticalSection    := @SysInitCriticalSection;
    DoneCriticalSection    := @SysDoneCriticalSection;
    EnterCriticalSection   := @SysEnterCriticalSection;
    LeaveCriticalSection   := @SysLeaveCriticalSection;
    InitThreadVar          := @SysInitThreadVar;
    RelocateThreadVar      := @SysRelocateThreadVar;
    AllocateThreadVars     := @SysAllocateThreadVars;
    ReleaseThreadVars      := nil;
    BasicEventCreate       := nil;
    BasicEventDestroy      := nil;
    BasicEventResetEvent   := nil;
    BasicEventSetEvent     := nil;
    BasiceventWaitFor      := nil;
    RTLEventCreate         := nil;
    RTLEventDestroy        := nil;
    RTLEventSetEvent       := nil;
    RTLEventResetEvent     := nil;
    RTLEventWaitFor        := nil;
    RTLEventWaitForTimeout := nil;
  end;
  SetThreadManager(ToroThreadManager);
  InitThreadVars(@SysRelocateThreadvar);
  // To check buffer this
  InitThread(200); 
end;

procedure MainEntry; [public, alias: '_mainCRTStartup']; assembler;
asm
call Init
call InitSystem
end;


procedure SystemExit; [public, alias : 'SYSTEMEXIT'];
begin
while true do;
end;



end.
