# ToroV
This repository contains the source code of ToroV, which allows applications to run as VMs.

## What is ToroV?
ToroV allows users to run an application as a VM. ToroV exposes a POSIX interface to the guest to enable to comunicate with the host. The architecture is split into the guest, the RTL and the VMM. The RTL is compiled with the guest and contains only the required code to boot the guest. The VMM intercepts the guest's syscalls. The VMM runs in the host. The VMM plays a similar role than QEMU but with less features and less footprint.   
