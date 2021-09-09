app="$1";
appsrc="$app.pas";
fpcrtlsource="$(eval echo ~$USER)/fpc-3.2.0-for-torov/rtl/";
# change to -O- for debugging
compileropti="-O2 -v0"
fpc -v0 -dFPC_NO_DEFAULT_MEMORYMANAGER -dHAS_MEMORYMANAGER -uFPC_HAS_INDIRECT_ENTRY_INFORMATION -dx86_64 -I$fpcrtlsource/objpas/sysutils/ -I$fpcrtlsource/linux/x86_64/ -I$fpcrtlsource/x86_64/ -I$fpcrtlsource/linux/ -I$fpcrtlsource/inc/ -I$fpcrtlsource/unix/ -Fu$fpcrtlsource/unix/ -Fu$fpcrtlsource/linux/ -MObjfpc $fpcrtlsource/linux/si_prc.pp -Fu$fpcrtlsource/objpas -Fu$fpcrtlsource/inc
fpc -v0 -Us -dx86_64 -I$fpcrtlsource/objpas/sysutils/ -I$fpcrtlsource/linux/x86_64/ -I$fpcrtlsource/x86_64/ -I$fpcrtlsource/linux/ -I$fpcrtlsource/inc/ -I$fpcrtlsource/unix/ -Fu$fpcrtlsource/unix -Fu$fpcrtlsource/linux -Fu$fpcrtlsource/objpas -Fu$fpcrtlsource/inc $fpcrtlsource/linux/system.pp
fpc -s -TLinux -I$fpcrtlsource/objpas/sysutils/ -I$fpcrtlsource/linux/x86_64 -I$fpcrtlsource/x86_64/ -I$fpcrtlsource/linux/ -I$fpcrtlsource/inc/ -I$fpcrtlsource/unix/ $compileropt -Xm -Si $compileropti $appsrc -o$app -Fu$fpcrtlsource/unix -Fu$fpcrtlsource/linux -Fu$fpcrtlsource/objpas -Fu$fpcrtlsource/inc -Fu../../src/rtl -MObjfpc -kprt0.o
/usr/bin/ld boot.o -s -Map $app.map -L. -o $app -T $app.ld -e _start 
