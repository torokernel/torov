// vmm.pas
//
// This program allows to run an application as a VM.
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
program vmm;

{$asmmode intel}
{$mode delphi}
{$MACRO ON}

uses BaseUnix, Linux, Kvm, sysutils, HyperCalls, Gdbstub;

const
  GUEST_ADDR_START = 0;
  GUEST_ADDR_MEM_SIZE = $200000;
  guestinitialregs : kvm_regs = ( rsp : GUEST_ADDR_MEM_SIZE; rip : GUEST_ADDR_START; rflags : 2 );
  CLONE_NEWPID = $20000000;
  STACK_SIZE = 1000;

var
  ret: LongInt;
  mem: PChar;
  guest: VM;
  guestVCPU: VCPU;
  exit_reason: LongInt;
  region, heap: kvm_userspace_memory_region;
  regs: kvm_regs;
  ioexit: PKvmRunExitIO;
  value: ^QWORD;
  ModeDebug: Boolean;
  DbgLocalPort: LongInt;
  debugexit: pkvm_debug_exit_arch;
  stack: array[0..STACK_SIZE] of byte;

// TODO: To check if binary fits
procedure LoadBinary(filemem: PChar; path: AnsiString);
var
  Buf : Array[1..2048] of byte;
  FBinary: File;
  ReadCount: LongInt;
begin
  Assign(FBinary, path);
  Reset(FBinary, 1);
  Repeat
    BlockRead (FBinary, Buf, Sizeof(Buf), ReadCount);
    move(Buf, filemem^, ReadCount);
    Inc(filemem, ReadCount);
  Until (ReadCount = 0);
  Close(FBinary);
end;

function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
begin
//  if ((Pointer(func)=nil) or (sp=nil)) then
//    exit(-1);
{$ASMMODE ATT}
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%rsi
        subl    $16,%rsi
        movl    args,%rax
        movl    %rax,8(%rsi)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%rax
        movl    %rax,0(%rsi)

        { Do the system call }
        pushl   %rdi
        movl    flags,%rdi
        movl    $56,%rax
        syscall
        popl    %rdi
        test    %rax,%rax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %rbp,%rbp       { terminate the stack frame }
        call    *%rdi
        { exit process }
        movl    %rax,%rdi
        movl    $60,%rax
        syscall
.Lclone_end:
        movl    %rax,__RESULT
  end;
end;

function chroot(path: pchar):longint;
begin
{$ASMMODE ATT}
  asm
        { Do the system call }
        pushl   %rdi
        movl    path,%rdi
        movl    $161,%rax
        syscall
        popl    %rdi
        movl    %rax,__RESULT
  end;
end;

function VMMLoop(arg: Pointer): LongInt;cdecl;
begin
  chroot('./mountpoint');
  chdir('/');
  while true do
  begin
    if not RunVCPU(@guestvcpu, exit_reason) then
    begin
      WriteLn('KVM_RUN');
      Break;
    end;
    if exit_reason = KVM_EXIT_HLT then
    begin
      //WriteLn('HLT!');
      ret := GetRegisters(@guestvcpu, @regs);
      if ret = -1 then
      begin
        WriteLn('KVM_GET_REGS');
        Break;
      end;
      if ModeDebug then
        BPHandler(@guestvcpu);
      //WriteLn('Halt instruction, rax: 0x', IntToHex(regs.rax, 4), ', rbx: 0x', IntToHex(regs.rbx, 4), ', rip: 0x', IntToHex(regs.rip, 4));
      Break;
    end else if exit_reason = KVM_EXIT_MMIO then
    begin
      continue;
    end else if exit_reason = KVM_EXIT_IO then
    begin
      ioexit := PKvmRunExitIO(@guestvcpu.run.padding_exit[0]);
      value := Pointer(PtrUInt(guestvcpu.run) + ioexit.data_offset);
      ret := GetRegisters(@guestvcpu, @regs);
      // WriteLn('IO: port: 0x', IntToHex(ioexit.port, 4), ', value: 0x', IntToHex(value^, 4), ', rbx: 0x', IntToHex(regs.rbx, 4), ', rcx: 0x', IntToHex(regs.rcx, 4));
      ret := HyperCallEntry(value^, @regs, @region, @heap);
      // set returned value
      regs.rax := ret;
      ConfigureRegs(@guestvcpu, @regs);
      continue;
    end else if exit_reason = KVM_EXIT_DEBUG then
    begin
      debugexit := pkvm_debug_exit_arch(@guestvcpu.run.padding_exit[0]);
      // WriteLn('Exit due to KVM_EXIT_DEBUG');
      if debugexit^.exception = 3 then
        BPHandler(@guestvcpu)
      else
        StepHandler(@guestvcpu);
      continue;
    end else
    begin
      ret := GetRegisters(@guestvcpu, @regs);
      WriteLn('exit_reason: rip: 0x', IntToHex(regs.rip, 4), ', reason: ', exit_reason);
      Break;
    end;
  end;
end;

var pid: TPid;

begin
  ModeDebug := false;
  for ret := 1 to ParamCount do
  begin
    if ParamStr(ret) = '-debug' then
    begin
      ModeDebug := true;
      DbgLocalPort := StrtoInt(ParamStr(ret+1));
    end;
  end;
  If not KvmInit then
  begin
    WriteLn('Unable to open /dev/kvm');
    Exit;
  end;
  guest.vmfd := CreateVM();
  if guest.vmfd = -1 then
  begin
    WriteLn('Error at CREATE_VM');
    Exit;
  end;
  // allocate memory for guest
  mem := fpmmap(nil, GUEST_ADDR_MEM_SIZE * 2, PROT_READ or PROT_WRITE, MAP_SHARED or MAP_ANONYMOUS, -1, 0);
  if mem = nil then
  begin
    WriteLn('Error at Allocating memory');
    Exit;
  end;
  guest.mem := mem;
  LoadBinary(mem, Paramstr(1));
  // set user memory region
  region.slot := 0;
  region.guest_phys_addr := GUEST_ADDR_START;
  region.memory_size := GUEST_ADDR_MEM_SIZE;
  region.userspace_addr := QWORD(mem);
  ret := SetUserMemoryRegion(guest.vmfd, @region);
  if ret = -1 then
  begin
    WriteLn('Error at KVM_SET_USER_MEMORY_REGION');
    Exit;
  end;
  // allocate heap
  heap.slot := 1;
  heap.guest_phys_addr := GUEST_ADDR_START + GUEST_ADDR_MEM_SIZE;
  heap.memory_size := GUEST_ADDR_MEM_SIZE;
  heap.userspace_addr := QWORD(mem) + GUEST_ADDR_MEM_SIZE;
  ret := SetUserMemoryRegion(guest.vmfd, @heap);
  if ret = -1 then
  begin
    WriteLn('Error at KVM_SET_USER_MEMORY_REGION');
    Exit;
  end;
  // vm is limited to one vcpu
  guestvcpu.vm := @guest;
  if not CreateVCPU(guest.vmfd, @guestvcpu) then
    Exit;
  // configure system registers
  if not ConfigureSregs(@guestvcpu) then
    Exit;
  // configure general purpose registers
  if not ConfigureRegs(@guestvcpu, @guestinitialregs) then
    Exit;
  // run VCPU in debug mode
  if ModeDebug then
  begin
    if not GdbstubInit(DbgLocalPort, @guestvcpu) then
    begin
      WriteLn('Error at GdbStubInit');
      Exit;
    end;
  end;
  pid := clone(VMMLoop, @stack[STACK_SIZE], CLONE_NEWPID or CLONE_VM or CLONE_FILES or CLONE_NEWNS or SIGCHLD, nil);
  FpWaitPid(pid, nil, 0);
  Fpmunmap(mem, GUEST_ADDR_MEM_SIZE * 2);
  fpClose(kvmfd);
end.
