# ToroV
ToroV enables applications to run as Virtual Machines. It is a Virtual Machine Monitor (VMM) that exposes a POSIX API to the guest. The guest communicates with the VMM by using syscalls.

## Architecture
The architecture is made of three components: the guest, the Runtime Library (RTL) and the VMM. The guest is a normal user application that is compiled with the RTL. The RTL contains the required code to run the application as a guest. For example, it allows to correctly boots the application. The guest is an user application that requires services from the OS by using syscalls. In ToroV, the VMM acts as the OS that provides such services. When the guest OS invokes a syscall, this produces a VMExit that the VMM catches, processes, and finally returns to the guest.

## How to try it?
You require to have install fpc-3.2.0.
You have to clone the RTL for torov from here.
To build the vmm, you have just to run build in ./vmm folder
Then, you cd to HelloWorld and you run ../build.sh HelloWorld
Check if the path are correct at HelloWorld.elf.ld
You will got three files HelloWorld.bin, HelloWorld.gdb and HelloWorld.elf.
To run the example, you have to run ./vmm HelloWorld.bin

## Contributions
