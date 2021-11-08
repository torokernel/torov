app="$1";
appsrc="$app.pas";
fpcrtlsource="$(eval echo ~$USER)/fpc-for-torov/rtl/";
# change to -O- for debugging
# compileropti="-g"
compileropti="-O2 -v0"
fpc -v0 -uFPC_HAS_INDIRECT_ENTRY_INFORMATION -dx86_64 -I$fpcrtlsource/objpas/sysutils/ -I$fpcrtlsource/linux/x86_64/ -I$fpcrtlsource/x86_64/ -I$fpcrtlsource/linux/ -I$fpcrtlsource/inc/ -I$fpcrtlsource/unix/ -Fu$fpcrtlsource/unix/ -Fu$fpcrtlsource/linux/ -MObjfpc $fpcrtlsource/linux/si_prc.pp -Fu$fpcrtlsource/objpas -Fu$fpcrtlsource/inc
fpc -v0 -Us -dx86_64 -I$fpcrtlsource/objpas/sysutils/ -I$fpcrtlsource/linux/x86_64/ -I$fpcrtlsource/x86_64/ -I$fpcrtlsource/linux/ -I$fpcrtlsource/inc/ -I$fpcrtlsource/unix/ -Fu$fpcrtlsource/unix -Fu$fpcrtlsource/linux -Fu$fpcrtlsource/objpas -Fu$fpcrtlsource/inc $fpcrtlsource/linux/system.pp
fpc -s -TLinux -I$fpcrtlsource/../packages/rtl-extra/src/inc -I$fpcrtlsource/../packages/rtl-extra/src/linux -I$fpcrtlsource/objpas/sysutils/ -I$fpcrtlsource/linux/x86_64 -I$fpcrtlsource/x86_64/ -I$fpcrtlsource/linux/ -I$fpcrtlsource/inc/ -I$fpcrtlsource/unix/ $compileropt -Xm -Si $compileropti $appsrc -o$app -Fu$fpcrtlsource/../packages/rtl-extra/src/unix -Fu$fpcrtlsource/unix -Fu$fpcrtlsource/linux -Fu$fpcrtlsource/objpas -Fu$fpcrtlsource/inc -Fu../../src/rtl -MObjfpc -kprt0.o
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64 boot.o -L. -o $app.elf -T $app.ld.elf -e _start
/usr/bin/objcopy -O binary $app.elf $app.bin
/usr/bin/objcopy --only-keep-debug $app.elf $app.dbg
