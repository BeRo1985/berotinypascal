@echo off
egasm +peexe +optimizejumps -gui rtl.asm
..\bin\btpc.exe < rtl2pas.dpr > rtl2pas.exe
rtl2pas.exe < rtl.exe > rtl.pas
