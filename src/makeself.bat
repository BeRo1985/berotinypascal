@echo off
btpc < btpc.dpr > btpcnew.exe
del btpc.exe
copy btpcnew.exe btpc.exe
del btpcnew.exe

