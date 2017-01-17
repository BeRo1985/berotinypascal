@echo off
egasm +peexe +optimizejumps -gui rtl.asm
dcc32 -B rtl2pas.dpr
rtl2pas

