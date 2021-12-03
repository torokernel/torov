# ToroV
ToroV enables applications to run as Virtual Machines. It is a Virtual Machine Monitor (VMM) that exposes a POSIX API to the guest. The guest communicates with the VMM by using syscalls.

## Architecture
The architecture is made of three components: the guest, the Runtime Library (RTL) and the VMM. The guest is a normal user application that is compiled with the RTL. The RTL contains the required code to run the application as a guest. For example, it allows to correctly boots the application. The guest is an user application that requires services from the OS by using syscalls. In ToroV, the VMM acts as the OS that provides such services. When the application guest invokes a syscall, this produces a VMExit that the VMM catches, processes, and finally returns to the application. This technology is similar than gVisor. The main difference with ToroV is that in gVisor syscalls are first trapped by the guest os, and then, forward them to the host. In ToroV, syscalls are trapped by the host first.

## Features
- Configurable syscalls per application
- Fast migration of applications
- Fast booting time
- Reduced memory footprint
- POSIX interface
- Fast syscalls

## Drawbacks
In ToroV, applications trigger a VMEXIT by using the **out** instruction. This instruction replaces the use of the **syscall** instruction. This requires that applications are compiled with a STDLIB in which the syscall instruction has been replaced.

## How to try it?
You require a Linux host with KVM to run the VMM. To check if KVM is enabled, you can execute **lsmod** to list the loaded module. If KVM is in the list, you can move forward, if not, you need to first install it.

## Try by using a ToroV Docker image (Recommended)
To simple try ToroV, you can build an image in docker with the required tools to build the vmm and the examples. Firs, you have to build the docker image by running:
```bash
wget https://raw.githubusercontent.com/torokernel/torov/master/ci/Dockerfile
docker build -t torov-dev .
```
Then, run the HelloWorld example by running:
```bash
docker run --privileged -it torov-dev
cd examples/HelloWorld
../build.sh HelloWorld
../../src/vmm/vmm helloworld.json
```
Note that docker runs with the `--priviliged` flag to be able to use Kvm from the container.

## Try step by step
### Step 0. Clone ToroV
```bash
git clone git@github.com:torokernel/torov.git
```
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
First, go to `torov/src/rtl` and execute:
```bash
fpc -s ToroVSys.pas
```
Second, go to `torov/examples` and execute:
```bash
nasm -f elf64 boot.s -o boot.o
```
Finally, go to `torov/examples/HelloWorld/HelloWorld.ld.elf` and edit the path to freepascal RTL objects. Then, run:
```bash
../build.sh HelloWorld
```
If the command successes, it generates three files: HelloWorld.elf, HelloWorld.bin and HelloWorld.dbg. You can run this example by running:
```bash
../../src/vmm/vmm ./helloworld.json
```
You will get something like:
```bash
Hello World, I am ToroV!
```
## How to debug an application
You can debug your application by using a gdb client. To do this, follow the steps:
### Step 1. Generate debug symbols
Edit `torov/examples/build.sh`, uncomment line 5, and comment line 6. Then, compile the HelloWorld example from its directory:
```bash
../build.sh HelloWorld
```
### Step 2. Run the VMM with a GdbStub
You have to edit helloworld.json and set the `Allowed` to true in the debug section. Then, run the VMM with the gdbstub:
```bash
../../src/vmm/vmm helloworld.json
```
The gdb server waits for the gdb client at port 1234.
### Step 3. Launch the gdb client
```bash
gdb HelloWorld.dbg
target remote localhost:1234
c
```
## Profiling
In the folder `torov/src/vmm`, you can find the script `profile.py` that is meant to measure the running time of any application. To run it, you have just to save the binary and the correspoding json in this directory, and then run the script as follows:
```bash
python3.5 ./profile.py 1000 helloworld.json
```
In this case, the test measures the average running time of 1000 executions of the HelloWorld example. The scripts outputs a gnuplot command to plot the result.
![plot](https://github.com/torokernel/torov/raw/master/examples/HelloWorld/HelloWorld.png)
## License
GPLv3
