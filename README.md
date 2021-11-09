# ToroV
ToroV enables applications to run as Virtual Machines. It is a Virtual Machine Monitor (VMM) that exposes a POSIX API to the guest. The guest communicates with the VMM by using syscalls.

## Architecture
The architecture is made of three components: the guest, the Runtime Library (RTL) and the VMM. The guest is a normal user application that is compiled with the RTL. The RTL contains the required code to run the application as a guest. For example, it allows to correctly boots the application. The guest is an user application that requires services from the OS by using syscalls. In ToroV, the VMM acts as the OS that provides such services. When the application guest invokes a syscall, this produces a VMExit that the VMM catches, processes, and finally returns to the application. This technology is similar than gVisor. The main difference with ToroV is that in gVisor syscalls are first trapped by the guest os, and then, forward them to the host. In ToroV, syscalls are trapped by the host first.

## Benefits
ToroV provides the following features:
- Fast booting time
- Reduced memory footprint
- POSIX interface
- Fast syscalls

## Drawbacks
In ToroV, applications trigger a VMEXIT by using the **out** instruction. This instruction replaces the use of the **syscall** instruction. This requires that applications are compiled with a STDLIB in which the syscall instruction has been replaced.

## How to try it?
You require a Linux host with KVM to run the VMM.

### Step 1. Install Freepascal 3.2.0
```bash
wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-laz_3.2.0-1_amd64.deb/download
mv download fpc-laz_3.2.0-1_amd64.deb
apt install ./fpc-laz_3.2.0-1_amd64.deb -y
```
### Step 2. Get the RTL for ToroV
```bash
git clone https://github.com/torokernel/freepascal.git -b fpc-3.2.0-for-torov fpc-3.2.0-for-torov
```
### Step 3. Edit path in build.sh
Go to `torov/examples` and edit `build.sh` to set the correct paths to fpc. The path corresponds with the directory where the freepascal compiler is stored in step 2.

### Step 4. Build the VMM
Go to `torov/src/vmm` and run `build.sh`. This generates the binary named `vmm` which contains the VMM.

### Step 5. Build HelloWorld example
Go to `torov/examples/HelloWorld/HelloWorld.ld.elf` and edit the path of the freepascal compiler. Then, run:
```bash
nasm -f elf64 boot.s -o boot.o
../build.sh HelloWorld
```
If the command successes, it generates three files: HelloWorld.elf, HelloWorld.bin and HelloWorld.dbg. You can run this example by running:
```bash
../../src/vmm/vmm HelloWorld.bin
```
You will get something like:
```bash
Hello World, I am ToroV!
Press a key and ENTER to finish
a
You pressed: a
Halt instruction, rax: 0x30A56, rbx: 0x2F8C0, rip: 0x0156
```
## How to debug an application
You can debug your application by using a gdb client. To do this, follow the steps:
### Step 1. Generate debug symbols
Edit `torov/examples/build.sh`, uncomment line 5, and comment line 6. Then, compile the HelloWorld example from its directory:
```bash
../build.sh HelloWorld
```
### Step 2. Run the VMM with a GdbStub
Run the VMM with the gdbstub:
```bash
../../src/vmm/vmm HelloWorld -debug 1234
```
The gdb server waits for the gdb client at port 1234.
### Step 3. Launch the gdb client
```bash
gdb HelloWorld.dbg
target remote localhost:1234
c
```
## Profiling HelloWorld
In the folder `torov/src/vmm`, you can find the script `profile.py` that is meant to measure the running time of the HelloWorld example. To run it, you have just to save the binary in this directory, and then run the script as follows:
```bash
python3.5 1000 HelloWorld
```
In this case, the test measures the average running time of 1000 executions of the HelloWorld example. The scripts ouputs a gnuplot command to plot the result.
![plot](https://github.com/torokernel/torov/raw/fixfornamespace/examples/HelloWorld/HelloWorld.png)
## License
GPLv3
