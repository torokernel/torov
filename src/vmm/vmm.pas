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

{$mode delphi}

uses BaseUnix, Linux, Kvm, Classes, strutils, sysutils,
     HyperCalls, Gdbstub, NameSpaces, fpjson, jsonparser;

const
  GUEST_ADDR_START = $400200;
  GUEST_ADDR_MEM_SIZE = $800000;
  GUEST_HEAP_ADDR = $600000;
  GUEST_HEAP_SIZE = $200000;
  STACK_SIZE = 2048;

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
  pid: TPid;
  mountpoint: array[0..100] of Char;
  flags: LongInt;

  // keep 256 bytes at the end of the stack
  guestinitialregs : kvm_regs = ( rsp : GUEST_ADDR_MEM_SIZE-256; rip : GUEST_ADDR_START; rflags : 2 );

  i: LongInt;
  jData: TJSONData;
  BinaryPath, hypc, Mountpoints: String;
  Input: TStream;
  jArray: TJSONArray;


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

function VMMLoop(args: Pointer): LongInt; cdecl;
begin
  Result := 0;
  if args <> nil then
  begin
    chroot(args);
    chdir('/');
  end;
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
      // WriteLn('Halt instruction, rax: 0x', IntToHex(regs.rax, 4), ', rbx: 0x', IntToHex(regs.rbx, 4), ', rip: 0x', IntToHex(regs.rip, 4));
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
      WriteLn('exit_reason: rip: 0x', IntToHex(regs.rip, 4), ' rbp: 0x', IntToHex(regs.rbp,4), ', reason: ', exit_reason);
      Break;
    end;
  end;
end;

begin
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ./vmm [json file]');
    Exit;
  end;
  ModeDebug := false;
  flags := 0;
  Input := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  jData := GetJSON(Input);
  if jData.FindPath('Binary') = Nil then
  begin
    WriteLn('JSON does not contain the name of the binary');
    Exit;
  end;
  if jData.FindPath('Startaddr') <> Nil then
  begin
    guestinitialregs.rip := Hex2Dec(jData.FindPath('Startaddr').AsString);
  end;
  BinaryPath := jData.FindPath('Binary').AsString;
  if jData.FindPath('MountPoint') <> nil then
  begin
    Mountpoints := jData.FindPath('MountPoint').AsString;
    flags := flags or CLONE_NEWNS;
    for i := 1 to Length(Mountpoints) do
    begin
      mountpoint[i-1] := Char(Mountpoints[i]);
    end;
    mountpoint[i] := #0;
  end;
  if jData.FindPath('Newpid') <> nil then
  begin
    if jData.FindPath('Newpid').AsBoolean then
    begin
      flags := flags or CLONE_NEWPID;
    end;
  end;
  jArray := TJSONArray(jData.FindPath('Hypercalls'));
  for i := 0 to Pred(jArray.Count) do
  begin
    if jData.FindPath('Hypercalls['+IntToStr(i)+'].Allowed').AsBoolean then
    begin
      hypc := jData.FindPath('Hypercalls['+IntToStr(i)+'].Name').AsString;
      if hypc = 'Write' then
      begin
        EnableHyperCall(syscall_nr_write);
      end else if hypc = 'Ioctl' then
      begin
        EnableHyperCall(syscall_nr_ioctl);
      end else if hypc = 'Getrlimit' then
      begin
        EnableHyperCall(syscall_nr_getrlimit);
      end else if hypc = 'Socket' then
      begin
        EnableHyperCall(syscall_nr_socket);
      end else if hypc = 'Bind' then
      begin
        EnableHyperCall(syscall_nr_bind);
      end else if hypc = 'Sendto' then
      begin
        EnableHyperCall(syscall_nr_sendto);
      end else if hypc = 'Recvfrom' then
      begin
        EnableHyperCall(syscall_nr_recvfrom);
      end else if hypc = 'Close' then
      begin
        EnableHyperCall(syscall_nr_close);
      end else if hypc = 'Accept' then
      begin
        EnableHyperCall(syscall_nr_accept);
      end else if hypc = 'Listen' then
      begin
        EnableHyperCall(syscall_nr_listen);
      end else if hypc = 'Open' then
      begin
        EnableHyperCall(syscall_nr_open);
      end else if hypc = 'Close' then
      begin
        EnableHyperCall(syscall_nr_close);
      end;

    end;
  end;
  if jData.FindPath('Debug.Allowed') <> Nil then
  begin
    if jData.FindPath('Debug.Allowed').AsBoolean then
    begin
      ModeDebug := true;
      DbgLocalPort := StrtoInt(jData.FindPath('Debug.Port').AsString);
    end;
  end;
  freemem(jData);
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
  mem := fpmmap(nil, GUEST_ADDR_MEM_SIZE, PROT_READ or PROT_WRITE, MAP_SHARED or MAP_ANONYMOUS, -1, 0);
  if mem = nil then
  begin
    WriteLn('Error at Allocating memory');
    Exit;
  end;
  guest.mem := mem;
  LoadBinary(PChar(PtrUInt(mem)+GUEST_ADDR_START), BinaryPath);

  // set user memory region
  region.slot := 0;
  region.guest_phys_addr := 0;
  region.memory_size := GUEST_ADDR_MEM_SIZE - GUEST_HEAP_SIZE;
  region.userspace_addr := QWORD(mem);
  ret := SetUserMemoryRegion(guest.vmfd, @region);
  if ret = -1 then
  begin
    WriteLn('Error at KVM_SET_USER_MEMORY_REGION');
    Exit;
  end;

  // allocate heap
  heap.slot := 1;
  heap.guest_phys_addr := GUEST_ADDR_MEM_SIZE - GUEST_HEAP_SIZE;
  heap.memory_size := GUEST_HEAP_SIZE;
  heap.userspace_addr := QWORD(mem) + GUEST_HEAP_ADDR;
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
  // map virtual guest memory
  if not ConfigureSregs(@guestvcpu, GUEST_ADDR_MEM_SIZE div PAGE_SIZE) then
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
  pid := clone(VMMLoop, @stack[STACK_SIZE], CLONE_VM or CLONE_FILES or SIGCHLD or flags, @mountpoint[0]);
  FpWaitPid(pid, nil, 0);
  Fpmunmap(mem, GUEST_ADDR_MEM_SIZE);
  fpClose(kvmfd);
end.
