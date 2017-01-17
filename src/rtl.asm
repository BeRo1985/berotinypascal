.CPU PENTIUM4
.BITS 32
.ENTRYPOINT
JMP StubEntryPoint
DSTR " Compiled by: BeRoTinyPascal - (C) Copyright 2006, Benjamin 'BeRo' Rosseaux "
HEAP_NO_SERIALIZE=1
HEAP_GENERATE_EXCEPTIONS=4
HEAP_ZERO_MEMORY=8
HEAP_CREATE_ALIGN_16=0x10000
RTLWriteCharBytesWritten: DB 0x90,0x8D,0x40,0x00
RTLWriteChar:
PUSH ESI
LEA ESI,[ESP+8]
PUSHAD
INVOKE WriteFile,DWORD PTR StdHandleOutput,ESI,BYTE 1,OFFSET RTLWriteCharBytesWritten,BYTE 0
POPAD
POP ESI
RET 4
RTLWriteIntegerBuffer: TIMES 3 DB 0x90,0x8D,0x40,0x00
RTLWriteInteger:
PUSH ESI
 MOV EBX,DWORD PTR [ESP+8]
 MOV EAX,DWORD PTR [ESP+12]
 CMP EAX,0
 JNL RTLWriteIntegerNotSigned
  NEG EAX
  DEC EBX
  PUSH BYTE '-'
  CALL RTLWriteChar
 RTLWriteIntegerNotSigned:
 XOR ECX,ECX
 PUSH EAX
 PUSH EBX
 RTLWriteIntegerPreCheckLoop:
  TEST EAX,EAX
  JZ RTLWriteIntegerPreCheckLoopDone
  INC ECX
  MOV EBX,10
  XOR EDX,EDX
  IDIV EBX
  JMP RTLWriteIntegerPreCheckLoop
 RTLWriteIntegerPreCheckLoopDone:
  TEST ECX,ECX
  SETZ DL
  OR CL,DL
 POP EBX
 POP EAX
 SUB EBX,ECX
 CMP EBX,0
 JLE RTLWriteIntegerNotPadding
  PUSH ECX
  RTLWriteIntegerPaddingLoop:
   PUSH BYTE ' '
   CALL RTLWriteChar
   DEC EBX
  JNZ RTLWriteIntegerPaddingLoop
  POP ECX
 RTLWriteIntegerNotPadding:
 LEA EDI,[OFFSET RTLWriteIntegerBuffer+ECX-1]
 PUSH ECX
 RTLWriteIntegerLoop:
  MOV ESI,10
  XOR EDX,EDX
  IDIV ESI
  LEA EBX,[EDX+'0']
  MOV BYTE PTR [EDI],BL
  DEC EDI
 LOOP RTLWriteIntegerLoop
 POP ECX
 INVOKE WriteFile,DWORD PTR StdHandleOutput,OFFSET RTLWriteIntegerBuffer,ECX,OFFSET RTLWriteCharBytesWritten,BYTE 0
POP ESI
RET 8
RTLWriteLn:
PUSH BYTE 13
CALL RTLWriteChar
PUSH BYTE 10
CALL RTLWriteChar
RET
ReadCharBuffer: DB 0x90
ReadCharBytesRead: DB 0x90,0x8D,0x40,0x00
ReadCharEx:
PUSHAD
INVOKE ReadFile,DWORD PTR StdHandleInput,OFFSET ReadCharBuffer,1,OFFSET ReadCharBytesRead,BYTE 0
TEST EAX,EAX
SETZ AL
OR BYTE PTR IsEOF,AL
CMP DWORD PTR [ReadCharBytesRead],0
SETZ AL
OR BYTE PTR IsEOF,AL
POPAD
RET
ReadCharInited: DB 0
ReadCharInit:
CMP BYTE PTR ReadCharInited,0
JNZ ReadInitDone
CALL ReadCharEx
MOV BYTE PTR ReadCharInited,1
ReadInitDone:
RET
RTLReadChar:
CALL ReadCharInit
MOVZX EAX,BYTE PTR ReadCharBuffer
CALL ReadCharEx
RET
RTLReadInteger:
CALL ReadCharInit
PUSHAD
 XOR EAX,EAX
 LEA ECX,[EAX+1]
 ReadIntegerSkipWhiteSpace:
  CMP BYTE PTR IsEOF,0
  JNZ ReadIntegerDone
  CMP BYTE PTR ReadCharBuffer,0
  JE ReadIntegerSkipWhiteSpaceDone
  CMP BYTE PTR ReadCharBuffer,32
  JA ReadIntegerSkipWhiteSpaceDone
  CALL ReadCharEx
  JMP ReadIntegerSkipWhiteSpace
 ReadIntegerSkipWhiteSpaceDone:
 CMP BYTE PTR ReadCharBuffer,'-'
 JNE ReadIntegerNotSigned
  NEG ECX
  CALL ReadCharEx
 ReadIntegerNotSigned:
 ReadIntegerLoop:
  MOVZX EBX,BYTE PTR ReadCharBuffer
  CMP BL,'0'
  JB ReadIntegerDone
  CMP BL,'9'
  JA ReadIntegerDone
  IMUL EAX,10
  LEA EAX,[EAX+EBX-'0']
  CALL ReadCharEx
  JMP ReadIntegerLoop
 ReadIntegerDone:
 IMUL ECX
 MOV DWORD PTR [ESP+28],EAX
POPAD
RET
RTLReadLn:
CALL ReadCharInit
CMP BYTE PTR IsEOF,0
JNE ReadLnDone
MOV BL,BYTE PTR ReadCharBuffer
CMP BL,10
JE ReadLnDone
CALL ReadCharEx
JMP RTLReadLn
ReadLnDone:
RET
IsEOF: DB 0
RTLEOF:
MOVZX EAX,BYTE PTR IsEOF
RET
RTLEOLN:
CMP BYTE PTR ReadCharBuffer,10
SETE DL
RET
OldStack: DB 0x90,0x8D,0x40,0x00
RTLHalt:
MOV ESP,DWORD PTR OldStack
INVOKE HeapFree,DWORD PTR HeapHandle,(HEAP_GENERATE_EXCEPTIONS+HEAP_ZERO_MEMORY+HEAP_CREATE_ALIGN_16)&HEAP_NO_SERIALIZE,DWORD PTR HeapMemory
INVOKE ExitProcess,BYTE 0
RTLCallHalt EQU DWORD PTR [ESI]
RTLCallWriteChar EQU DWORD PTR [ESI+4]
RTLCallWriteInteger EQU DWORD PTR [ESI+8]
RTLCallWriteLn EQU DWORD PTR [ESI+12]
RTLCallReadChar EQU DWORD PTR [ESI+16]
RTLCallReadInteger EQU DWORD PTR [ESI+20]
RTLCallReadLn EQU DWORD PTR [ESI+24]
RTLCallEOF EQU DWORD PTR [ESI+28]
RTLCallEOLN EQU DWORD PTR [ESI+32]
RTLFunctionTable:
DD OFFSET RTLHalt
DD OFFSET RTLWriteChar
DD OFFSET RTLWriteInteger
DD OFFSET RTLWriteLn
DD OFFSET RTLReadChar
DD OFFSET RTLReadInteger
DD OFFSET RTLReadLn
DD OFFSET RTLEOF
DD OFFSET RTLEOLN
HeapHandle: DB 0x90,0x8D,0x40,0x00
HeapMemory: DB 0x90,0x8D,0x40,0x00
StdHandleInput: DB 0x90,0x8D,0x40,0x00
StdHandleOutput: DB 0x90,0x8D,0x40,0x00
StubEntryPoint:
INVOKE GetStdHandle,BYTE -10
MOV DWORD PTR StdHandleInput,EAX
INVOKE SetConsoleMode,EAX,1+4
INVOKE GetStdHandle,BYTE -11
MOV DWORD PTR StdHandleOutput,EAX
INVOKE SetConsoleMode,EAX,1+2
MOV DWORD PTR OldStack,ESP
INVOKE GetProcessHeap
MOV DWORD PTR HeapHandle,EAX
INVOKE HeapAlloc,EAX,HEAP_GENERATE_EXCEPTIONS+HEAP_ZERO_MEMORY+HEAP_CREATE_ALIGN_16,4194332
MOV DWORD PTR HeapMemory,EAX
LEA ESP,[EAX+4194304]
MOV EBP,ESP
MOV ESI,OFFSET RTLFunctionTable
.LIBRARY "kernel32.dll"
IMPORT ExitProcess "ExitProcess"
IMPORT GetStdHandle "GetStdHandle"
IMPORT SetConsoleMode "SetConsoleMode"
IMPORT WriteFile "WriteFile"
IMPORT ReadFile "ReadFile"
IMPORT ReadConsoleInputA "ReadConsoleInputA"
IMPORT GetProcessHeap "GetProcessHeap"
IMPORT HeapAlloc "HeapAlloc"
IMPORT HeapFree "HeapFree"
ProgramEntryPoint: