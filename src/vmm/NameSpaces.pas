// NameSpaces.pas
//
// This unit implements the minimal infrastructure that allows the vmm
// to be isoalted in term of namespaces.
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
Unit NameSpaces;



interface

uses BaseUnix, Linux;

const
  CLONE_NEWPID = $20000000;

function chroot(path: pchar):longint;
function clone(func:TCloneFunc;sp:pointer;flags:QWORD;args:pointer):QWORD;

implementation

// clone:
// This function implements the clone syscall
//
function clone(func:TCloneFunc;sp:pointer;flags:qword;args:pointer):QWORD;
begin
{$ASMMODE ATT}
  asm
        { Insert the argument onto the new stack. }
        movq    sp,%rsi
        subq    $16,%rsi
        movq    args,%rax
        movq    %rax,8(%rsi)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movq    func,%rax
        movq    %rax,0(%rsi)

        { Do the system call }
        pushq   %rdi
        movq   flags,%rdi
        movq    $56,%rax
        syscall
        popq    %rdi
        test    %rax,%rax
        jnz     .Lclone_end

        { We're in the new thread }
        subq    %rbp,%rbp       { terminate the stack frame }
        movq     %rdi, %rax
        // FPC ignores cdecl so just use normal convention
        popq     %rdi
        call    *%rax
        { exit process }
        movq    %rax,%rdi
        movq    $60,%rax
        syscall
.Lclone_end:
        movq    %rax,__RESULT
  end;
end;

// chroot:
// This function implements the chroot syscall
//
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
end.
