// Kvm.pas
//
// This unit allows to manage VMs by using the KVM API.
//
// Copyright (c) 2021 Matias Vara <matiasevara@torokernel.com>
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
unit Kvm;

interface

{$mode delphi}
uses BaseUnix, Linux;

const
  KVM_EXIT_IO_IN = 0;
  KVM_EXIT_IO_OUT = 1;
  SYNC_REGS_SIZE_BYTES = 2048;
  KVM_GET_API_VERSION = $AE00;
  KVM_RUN_A = $AE80;
  KVM_GET_VCPU_MMAP_SIZE = $AE04;
  KVM_CREATE_VM = $AE01;
  KVM_SET_USER_MEMORY_REGION = $4020ae46;
  KVM_CREATE_VCPU = $AE41;
  KVM_GET_SREGS = $8138ae83;
  KVM_SET_SREGS = $4138ae84;
  KVM_NR_INTERRUPTS = 256;
  KVM_GET_REGS = $8090ae81;
  KVM_SET_REGS = $4090ae82;
  KVM_EXIT_HLT = 5;
  KVM_EXIT_MMIO = 6;
  KVM_EXIT_IO = 2;
  KVM_EXIT_DEBUG = 4;

  PDE64_PRESENT = 1;
  PDE64_RW = 1 shl 1;
  PDE64_USER = 1 shl 2;
  PDE64_PS = 1 shl 7;
  CR0_PE = 1;
  CR0_MP = 1 shl 1;
  CR0_ET = 1 shl 4;
  CR0_NE = 1 shl 5;
  CR0_WP = 1 shl 16;
  CR0_AM = 1 shl 18;
  CR0_PG = 1 shl 31;
  EFER_LME = 1 shl 8;
  EFER_LMA = 1 shl 10;
  CR4_PAE = 1 shl 5;

  KVM_GUESTDBG_ENABLE = 1;
  KVM_GUESTDBG_SINGLESTEP = 2;
  KVM_GUESTDBG_USE_SW_BP = $10000;
  KVM_SET_GUEST_DEBUG = $4048ae9b;

  PAGE_SIZE = $200000;

type
  pkvm_user_memory_region = ^kvm_userspace_memory_region;
  kvm_userspace_memory_region = record
    slot: DWORD;
    flags: DWORD;
    guest_phys_addr: QWORD;
    memory_size: QWORD;
    userspace_addr: QWORD;
  end;

  // KVM_EXIT_UNKNOWN
  kvm_run_hw = record
    hardware_exit_reason: QWORD;
  end;

  // KVM_EXIT_FAIL_ENTRY
  kvm_run_fail_entry = record
    hardware_entry_failure_reason: QWORD;
	  cpu: DWORD;
  end;

  // KVM_EXIT_EXCEPTION
  kvm_run_ex = record
    exception: DWORD;
	  error_code: DWORD;
  end;

  // KVM_EXIT_IO
  PKvmRunExitIO = ^kvm_run_exit_io;
  kvm_run_exit_io = record
    direction: Byte;
	  size: Byte;
	  port: WORD;
	  count: DWORD;
	  data_offset: QWORD;
  end;

  // KVM_EXIT_MMIO
  kvm_run_mmio = record
    phys_addr: QWORD;
	  data: array[0..7] of Byte;
	  len: DWORD;
	  is_write: Byte;
  end;

  // KVM_EXIT_INTERNAL_ERROR
  kvm_run_internal = record
    suberror: DWORD;
	  ndata: DWORD;
	  data: array[0..15] of QWORD;
  end;

  PKvmRun = ^kvm_run;
  kvm_run = record
	  // in
    request_interrupt_window: Byte;
	  immediate_exit: Byte;
	  padding1: array[0..5] of Byte;

	  // out
	  exit_reason: DWORD;
 	  ready_for_interrupt_injection: Byte;
	  if_flag: Byte;
	  flags: WORD;

	  // in (pre_kvm_run), out (post_kvm_run)
	  cr8: QWORD;
	  apic_base: QWORD;

	  // this contain the union structure
	  padding_exit: array[0..255] of Char;

	  kvm_valid_regs: QWORD;
	  kvm_dirty_regs: QWORD;
	  padding: array[0..SYNC_REGS_SIZE_BYTES-1] of Char;
  end;

  kvm_segment = record
    base: QWORD;
    limit: DWORD;
    selector: WORD;
    tp: Byte;
    present, dpl, db, s, l, g, avl: Byte;
    unusable: Byte;
    padding: Byte;
  end;

  kvm_dtable = record
    base: QWORD;
    limit: WORD;
    padding: array[0..2] of WORD;
  end;

  pkvm_sregs = ^kvm_sregs;
  kvm_sregs = record
    cs, ds, es, fs, gs, ss: kvm_segment;
    tr, ldt: kvm_segment;
    gdt, idt: kvm_dtable;
    cr0, cr2, cr3, cr4, cr8: QWORD;
    efer: QWORD;
    apic_base: QWORD;
    interrupt_bitmap: array[0..((KVM_NR_INTERRUPTS + 63) div 64)-1] of QWORD;
  end;

  pkvmregs = ^kvm_regs;
  kvm_regs = record
    rax, rbx, rcx, rdx: QWORD;
    rsi, rdi, rsp, rbp: QWORD;
    r8,  r9,  r10, r11: QWORD;
    r12, r13, r14, r15: QWORD;
    rip, rflags: QWORD;
  end;

  PVM = ^VM;
  VM = record
    // TODO: this can just LongInt
    vmfd: LongInt;
    mem: Pointer;
  end;

  PVCPU = ^VCPU;
  VCPU = record
    vm: PVM;
    vcpufd: LongInt;
    run: PKvmRun;
    sregs: kvm_sregs;
  end;

  kvm_guest_debug = record
    control: DWORD;
    pad: DWORD;
    debugreg: array[0..7] of QWORD;
  end;

  pkvm_debug_exit_arch = ^kvm_debug_exit_arch;
  kvm_debug_exit_arch = record
    exception: DWORD;
    pad: DWORD;
    pc: QWORD;
    dr6: QWORD;
    dr7: QWORD;
  end;

function KvmInit: Boolean;
function CreateVM: LongInt;
function SetUserMemoryRegion(vmfd: LongInt; region: pkvm_user_memory_region): LongInt;
function CreateVCPU(vmfd: LongInt; vcpu: PVCPU): Boolean;
function ConfigureSregs(vcpu: PVCPU; nr: LongInt): Boolean;
function ConfigureRegs(vcpu: PVCPU;regs: pkvmregs): Boolean;
function RunVCPU(vcpu: PVCPU; Out Reason: Longint): Boolean;
function GetRegisters(vcpu: PVCPU; regs: pkvmregs): LongInt;
function SetupDebugGuest(vcpu: PVCPU) : Boolean;
function SetDebugGuestStep(vcpu: PVCPU) : Boolean;
function ClearDebugGuest(vcpu:PVCPU): Boolean;

var
  kvmfd: LongInt;

implementation

function GetRegisters(vcpu: PVCPU; regs: pkvmregs): LongInt;
begin
  Result := fpIOCtl(vcpu.vcpufd, KVM_GET_REGS, regs);
end;

function CreateVM: LongInt;
begin
  Result := fpIOCtl(kvmfd, KVM_CREATE_VM, nil);
end;

function SetUserMemoryRegion(vmfd: LongInt; region: pkvm_user_memory_region): Longint;
begin
  Result := fpIOCtl(vmfd, KVM_SET_USER_MEMORY_REGION, region);
end;

procedure SetupLongMode(mem: Pointer; sregs: pkvm_sregs; nr: Longint);
var
  pml4, pdpt, pd: ^QWORD;
  seg: kvm_segment;
  i, page: LongInt;
begin
  pml4 := Pointer(PtrUInt(mem)+$2000);
  pdpt := Pointer(PtrUInt(mem)+$3000);
  pd := Pointer(PtrUInt(mem)+$4000);

  pml4^ := PDE64_PRESENT or PDE64_RW or PDE64_USER or $3000;
  pdpt^ := PDE64_PRESENT or PDE64_RW or PDE64_USER or $4000;

  page := 0;

  // map nr pages
  for i := 0 to nr-1 do
  begin
    pd^ := PDE64_PRESENT or PDE64_RW or PDE64_USER or PDE64_PS or page;
    Inc(page, PAGE_SIZE);
    Inc(pd);
  end;

  // this supposes that we start at 0
  sregs.cr3 := $2000;
  sregs.cr4 := CR4_PAE;
  sregs.cr0 := CR0_PE or CR0_MP or CR0_ET or CR0_NE or CR0_WP or CR0_AM or CR0_PG;
  sregs.efer := EFER_LME or EFER_LMA;

  seg.base := 0;
  seg.limit := $ffffffff;
  seg.selector := 1 shl 3;
  seg.present := 1;
  seg.tp := 11;
  seg.dpl := 0;
  seg.db := 0;
  seg.s := 1;
  seg.l := 1;
  seg.g := 1;

  sregs^.cs := seg;

  seg.tp := 3;
  seg.selector := 2 shl 3;
  sregs.ds := seg;
  sregs.fs := seg;
  sregs.gs := seg;
  sregs.ss := seg;
end;

function SetupDebugGuest(vcpu: PVCPU) : Boolean;
var
  debug: kvm_guest_debug;
  ret: LongInt;
begin
  Result := False;
  fillbyte(debug, sizeof(debug), 0);
  debug.control := KVM_GUESTDBG_ENABLE or KVM_GUESTDBG_USE_SW_BP;
  ret := fpIOCtl(vcpu.vcpufd, KVM_SET_GUEST_DEBUG, @debug);
  if ret = -1 then
  begin
    WriteLn('SetupDebugGuest: Error at KVM_SET_GUEST_DEBUG');
    Exit;
  end;
  Result := True;
end;

function SetDebugGuestStep(vcpu: PVCPU) : Boolean;
var
  debug: kvm_guest_debug;
  ret: LongInt;
begin
  Result := False;
  fillbyte(debug, sizeof(debug), 0);
  debug.control := KVM_GUESTDBG_ENABLE or KVM_GUESTDBG_USE_SW_BP or KVM_GUESTDBG_SINGLESTEP;
  ret := fpIOCtl(vcpu.vcpufd, KVM_SET_GUEST_DEBUG, @debug);
  if ret = -1 then
  begin
    WriteLn('SetupDebugGuest: Error at KVM_SET_GUEST_DEBUG');
    Exit;
  end;
  Result := True;
end;

function ClearDebugGuest(vcpu: PVCPU) : Boolean;
var
  debug: kvm_guest_debug;
  ret: LongInt;
begin
  Result := False;
  fillbyte(debug, sizeof(debug), 0);
  ret := fpIOCtl(vcpu.vcpufd, KVM_SET_GUEST_DEBUG, @debug);
  if ret = -1 then
  begin
    WriteLn('SetupDebugGuest: Error at KVM_SET_GUEST_DEBUG');
    Exit;
  end;
  Result := True;
end;

function KvmInit: Boolean;
var
  ret: LongInt;
begin
  Result := false;
  kvmfd := fpOpen('/dev/kvm', O_RdWr or O_CLOEXEC);
  if kvmfd < 0 then
  begin
    WriteLn('KvmInit: error!');
    Exit;
  end;
  ret := fpIOCtl(kvmfd, KVM_GET_API_VERSION, nil);
  if ret = -1 then
  begin
    WriteLn('KvmInit: Error at KVM_GET_API_VERSION');
    Exit;
  end;
  if ret <> 12 then
  begin
    WriteLn('KvmInit: KVM_GET_API_VERSION ', ret, ', expected 12');
    Exit;
  end;
  Result := True;
end;

function CreateVCPU(vmfd: LongInt; vcpu: PVCPU): Boolean;
var
  vcpufd: LongInt;
  run: ^kvm_run;
  mmapsize, ret: LongInt;
begin
  Result := false;
  vcpufd := fpIOCtl(vmfd, KVM_CREATE_VCPU, nil);
  if vcpufd = -1 then
  begin
    WriteLn('CreateVCPU: Error at KVM_CREATE_VCPU');
    Exit;
  end;
  ret := fpIOCtl(kvmfd, KVM_GET_VCPU_MMAP_SIZE, nil);
  if ret = -1 then
  begin
    WriteLn('CreateVCPU: Error at KVM_GET_VCPU_MMAP_SIZE');
    Exit;
  end;
  mmapsize := ret;
  if mmapsize < sizeof(run^) then
  begin
    WriteLn('CreateVCPU: Error at KVM_GET_VCPU_MMAP_SIZE unexpectedly small');
    Exit;
  end;
  run := fpmmap(nil, mmapsize, PROT_READ or PROT_WRITE, MAP_SHARED, vcpufd, 0);
  if run = nil then
  begin
    WriteLn('CreateVCPU: error at mmap run');
    Exit;
  end;
  vcpu.vcpufd := vcpufd;
  vcpu.run := run;
  Result := true;
end;

function ConfigureSregs(vcpu: PVCPU; nr: LongInt): Boolean;
var
  ret: LongInt;
begin
  Result := false;
  ret := fpIOCtl(vcpu.vcpufd, KVM_GET_SREGS, @vcpu.sregs);
  if ret = -1 then
  begin
    WriteLn('ConfigureSregs: Error at KVM_GET_SREGS');
    Exit;
  end;
  SetupLongmode(vcpu.vm.mem, @vcpu.sregs, nr);
  ret := fpIOCtl(vcpu.vcpufd, KVM_SET_SREGS, @vcpu.sregs);
  if ret = -1 then
  begin
    WriteLn('KVM_SET_SREGS');
    Exit;
  end;
  Result := true;
end;

function ConfigureRegs(vcpu: PVCPU; regs: pkvmregs): Boolean;
var
  ret: LongInt;
begin
  Result := False;
  ret := fpIOCtl(vcpu.vcpufd, KVM_SET_REGS, regs);
  if ret = -1 then
  begin
    WriteLn('ConfigureRegs: KVM_SET_REGS');
    Exit;
  end;
  Result := True;
end;

function RunVCPU(vcpu: PVCPU; out Reason: Longint): Boolean;
var
  ret: LongInt;
begin
  Result := False;
  ret := fpIOCtl(vcpu.vcpufd, KVM_RUN_A, nil);
  If ret = -1 then
  begin
    WriteLn('RunVCPU: error');
    Exit;
  end;
  Reason := vcpu.run.exit_reason;
  Result := True;
end;

end.
