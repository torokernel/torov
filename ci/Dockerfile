# syntax=docker/dockerfile:1
FROM debian:buster
RUN apt update && apt install nasm git wget -y && wget https://sourceforge.net/projects/lazarus/files/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.10/fpc-laz_3.2.0-1_amd64.deb/download && mv download fpc-laz_3.2.0-1_amd64.deb && apt install ./fpc-laz_3.2.0-1_amd64.deb -y
RUN ln -s /root /home/debian
WORKDIR /root
RUN git clone https://github.com/torokernel/freepascal.git -b fpc-3.2.0-for-torov fpc-for-torov
RUN git clone https://github.com/torokernel/torov.git
WORKDIR /root/torov
RUN fpc -s src/rtl/ToroVSys.pas && nasm -f elf64 examples/boot.s -o examples/boot.o && cd ./src/vmm && ./build.sh
