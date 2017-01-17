(******************************************************************************
 *                                 BeRoTinyPascal                             *
 ******************************************************************************
 *   A self-hosting capable tiny pascal compiler for the Win32 x86 platform   *
 ******************************************************************************
 *                        Version 2016-06-22-18-07-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2006-2016, Benjamin Rosseaux (benjamin@rosseaux.com)         *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/berotinypascal .                             *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 *
 *    and even with BeRoTinyPascal itself, so don't use generics/templates,   *
 *    operator overloading and another newer syntax features than Delphi 7    *
 *    and BeRoTinyPascal have support for that.                               *
 * 5. Don't use any libraries/units except the RTL system unit functions      *
 * +. Make sure the code compiles with Delphi 7, FreePascal >= 3.0 and with   *
 *    BeRoTinyPascal itself.                                                  * 
 *                                                                            *
 ******************************************************************************)
program BTPC; { BeRoTinyPascalCompiler }
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$ifdef Windows}
 {$apptype console}
{$endif}

const MaximalCodeSize=262144;
      MaximalIdentifiers=512;
      MaximalTypes=32;
      MaximalList=10;
      MaximalAlfa=20;
      MaximalStringLength=255;
      MaximalCases=256;

      OPNone=-1;
      OPAdd=0;
      OPNeg=1;
      OPMul=2;
      OPDivD=3;
      OPRemD=4;
      OPDiv2=5;
      OPRem2=6;
      OPEqlI=7;
      OPNEqI=8;
      OPLssI=9;
      OPLeqI=10;
      OPGtrI=11;
      OPGEqI=12;
      OPDupl=13;
      OPSwap=14;
      OPAndB=15;
      OPOrB=16;
      OPLoad=17;
      OPStore=18;
      OPHalt=19;
      OPWrI=20;
      OPWrC=21;
      OPWrL=22;
      OPRdI=23;
      OPRdC=24;
      OPRdL=25;
      OPEOF=26;
      OPEOL=27;
      OPLdC=28;
      OPLdA=29;
      OPLdLA=30;
      OPLdL=31;
      OPLdG=32;
      OPStL=33;
      OPStG=34;
      OPMove=35;
      OPCopy=36;
      OPAddC=37;
      OPMulC=38;
      OPJmp=39;
      OPJZ=40;
      OPCall=41;
      OPAdjS=42;
      OPExit=43;

      TokIdent=0;
      TokNumber=1;
      TokStrC=2;
      TokPlus=3;
      TokMinus=4;
      TokMul=5;
      TokLBracket=6;
      TokRBracket=7;
      TokColon=8;
      TokEql=9;
      TokNEq=10;
      TokLss=11;
      TokLEq=12;
      TokGtr=13;
      TokGEq=14;
      TokLParent=15;
      TokRParent=16;
      TokComma=17;
      TokSemi=18;
      TokPeriod=19;
      TokAssign=20;
      SymBEGIN=21;
      SymEND=22;
      SymIF=23;
      SymTHEN=24;
      SymELSE=25;
      SymWHILE=26;
      SymDO=27;
      SymCASE=28;
      SymREPEAT=29;
      SymUNTIL=30;
      SymFOR=31;
      SymTO=32;
      SymDOWNTO=33;
      SymNOT=34;
      SymDIV=35;
      SymMOD=36;
      SymAND=37;
      SymOR=38;
      SymCONST=39;
      SymVAR=40;
      SymTYPE=41;
      SymARRAY=42;
      SymOF=43;
      SymPACKED=44;
      SymRECORD=45;
      SymPROGRAM=46;
      SymFORWARD=47;
      SymHALT=48;
      SymFUNC=49;
      SymPROC=50;

      IdCONST=0;
      IdVAR=1;
      IdFIELD=2;
      IdTYPE=3;
      IdFUNC=4;

      KindSIMPLE=0;
      KindARRAY=1;
      KindRECORD=2;

      TypeINT=1;
      TypeBOOL=2;
      TypeCHAR=3;
      TypeSTR=4;

      FunCHR=0;
      FunORD=1;
      FunWRITE=2;
      FunWRITELN=3;
      FunREAD=4;
      FunREADLN=5;
      FunEOF=6;
      FunEOFLN=7;

type TAlfa=array[1..MaximalAlfa] of char;

     TIdent=record
      Name:TAlfa;
      Link:integer;
      TypeDefinition:integer;
      Kind:integer;
      Value:integer;
      VariableLevel:integer;
      VariableAddress:integer;
      ReferencedParameter:boolean;
      Offset:integer;
      FunctionLevel:integer;
      FunctionAddress:integer;
      LastParameter:integer;
      ReturnAddress:integer;
      Inside:boolean;
     end;

     TType=record
      Size:integer;
      Kind:integer;
      StartIndex:integer;
      EndIndex:integer;
      SubType:integer;
      Fields:integer;
     end;

var CurrentChar:char;
    CurrentColumn:integer;
    CurrentLine:integer;
    CurrentSymbol:integer;
    CurrentIdentifer:TAlfa;
    CurrentNumber:integer;
    CurrentString:array[1..255] of char;
    CurrentStringLength:integer;
    FunctionDeclarationIndex:integer;
    Keywords:array[SymBEGIN..SymPROC] of TAlfa;
    LastOpcode:integer;
    CurrentLevel:integer;
    IsLabeled:boolean;
    SymbolNameList:array[-1..MaximalList] of integer;
    IdentifierPosition:integer;
    TypePosition:integer;
    Identifiers:array[0..MaximalIdentifiers] of TIdent;
    Types:array[1..MaximalTypes] of TType;
    Code:array[0..MaximalCodeSize] of integer;
    CodePosition:integer;
    StackPosition:integer;

function StringCompare(var s1,s2:TAlfa):boolean;
var f:boolean;
    i:integer;
begin
 f:=true;
 i:=1;
 while f and (i<=MaximalAlfa) do begin
  f:=(s1[i]=s2[i]);
  i:=i+1;
 end;
 StringCompare:=f;
end;

procedure StringCopy(var Dest:TAlfa;Src:TAlfa);
begin
 Dest:=Src;
end;

procedure Error(n:integer);
begin
 Write('Error ',n:1,': ');
 case n of
  TokIdent:begin
   Write('Identifier expected');
  end;
  TokNumber:begin
   Write('Number expected');
  end;
  TokStrC:begin
   Write('String expected');
  end;
  TokPlus:begin
   Write('"+" expected');
  end;
  TokMinus:begin
   Write('"-" expected');
  end;
  TokMul:begin
   Write('"*" expected');
  end;
  TokLBracket:begin
   Write('"[" expected');
  end;
  TokRBracket:begin
   Write('"]" expected');
  end;
  TokColon:begin
   Write('":" expected');
  end;
  TokEql:begin
   Write('"=" expected');
  end;
  TokNEq:begin
   Write('"<>" expected');
  end;
  TokLss:begin
   Write('"<" expected');
  end;
  TokLEq:begin
   Write('"<=" expected');
  end;
  TokGtr:begin
   Write('">" expected');
  end;
  TokGEq:begin
   Write('">=" expected');
  end;
  TokLParent:begin
   Write('"(" expected');
  end;
  TokRParent:begin
   Write('")" expected');
  end;
  TokComma:begin
   Write('"," expected');
  end;
  TokSemi:begin
   Write('";" expected');
  end;
  TokPeriod:begin
   Write('"." expected');
  end;
  TokAssign:begin
   Write('":=" expected');
  end;
  SymBEGIN:begin
   Write('"begin" expected');
  end;
  SymEND:begin
   Write('"end" expected');
  end;
  SymIF:begin
   Write('"if" expected');
  end;
  SymTHEN:begin
   Write('"then" expected');
  end;
  SymELSE:begin
   Write('"else" expected');
  end;
  SymWHILE:begin
   Write('"else" expected');
  end;
  SymDO:begin
   Write('"do" expected');
  end;
  SymCASE:begin
   Write('"case" expected');
  end;
  SymREPEAT:begin
   Write('"repeat" expected');
  end;
  SymUNTIL:begin
   Write('"until" expected');
  end;
  SymFOR:begin
   Write('"for" expected');
  end;
  SymTO:begin
   Write('"to" expected');
  end;
  SymDOWNTO:begin
   Write('"downto" expected');
  end;
  SymNOT:begin
   Write('"not" expected');
  end;
  SymDIV:begin
   Write('"div" expected');
  end;
  SymMOD:begin
   Write('"mod" expected');
  end;
  SymAND:begin
   Write('"and" expected');
  end;
  SymOR:begin
   Write('"or" expected');
  end;
  SymCONST:begin
   Write('"const" expected');
  end;
  SymVAR:begin
   Write('"var" expected');
  end;
  SymTYPE:begin
   Write('"type" expected');
  end;
  SymARRAY:begin
   Write('"array" expected');
  end;
  SymOF:begin
   Write('"of" expected');
  end;
  SymPACKED:begin
   Write('"packed" expected');
  end;
  SymRECORD:begin
   Write('"record" expected');
  end;
  SymPROGRAM:begin
   Write('"program" expected');
  end;
  SymFORWARD:begin
   Write('"forward" expected');
  end;
  SymHALT:begin
   Write('"halt" expected');
  end;
  SymFUNC:begin
   Write('"function" expected');
  end;
  SymPROC:begin
   Write('"procedure" expected');
  end;
  100:begin
   Write('String literal must be closed');
  end;
  101:begin
   Write('String is empty');
  end;
  102:begin
   Write('Bad char');
  end;
  103:begin
   Write('Too many identifiers');
  end;
  104:begin
   Write('Duplicate identifier');
  end;
  105:begin
   Write('Duplicate procedure/function');
  end;
  106:begin
   Write('Unknown identifiers');
  end;
  107:begin
   Write('Invalid type');
  end;
  108:begin
   Write('Record type expected');
  end;
  109:begin
   Write('Unknown field');
  end;
  110:begin
   Write('Array type expected');
  end;
  111:begin
   Write('Non-writeable type');
  end;
  112:begin
   Write('Non-readable type');
  end;
  113:begin
   Write('Too many argumnts');
  end;
  114:begin
   Write('Passing string to var argument isn''t allowed');
  end;
  115:begin
   Write('Passing string to non-array argument isn''t allowed');
  end;
  116:begin
   Write('Passing string to non-char-array argument isn''t allowed');
  end;
  117:begin
   Write('Passing string to wrong sized char-array argument isn''t allowed');
  end;
  118:begin
   Write('Too few argumnts');
  end;
  119:begin
   Write('Procedure calls inside a expression aren''t allowed');
  end;
  120:begin
   Write('Type inside a expression isn''t allowed');
  end;
  121:begin
   Write('Expression expected');
  end;
  122:begin
   Write('Illegal assigning to function');
  end;
  123:begin
   Write('Illegal assigning to constant or type');
  end;
  124:begin
   Write('Case expression must be constant');
  end;
  125:begin
   Write('Case expression expected');
  end;
  126:begin
   Write('":" expected');
  end;
  127:begin
   Write('Variable after "FOR" expected');
  end;
  128:begin
   Write('Incorrect iterator type');
  end;
  129:begin
   Write('"TO" or "DOWNTO" expected');
  end;
  130:begin
   Write('Constant expected');
  end;
  131:begin
   Write('Identifier or number literal expected');
  end;
  132:begin
   Write('First index of array must be less or equal then last');
  end;
  133:begin
   Write('Type expected');
  end;
  134:begin
   Write('Too many types');
  end;
  135:begin
   Write('Too many nested records');
  end;
  136:begin
   Write('Too many nested procedures');
  end;
  137:begin
   Write('Invalid function return type');
  end;
  138:begin
   Write('Too many arguments then at forward declaration');
  end;
  139:begin
   Write('Argument name doesn''t match forward declaration');
  end;
  140:begin
   Write('Argument type doesn''t match forward declaration');
  end;
  141:begin
   Write('Argument var doesn''t match forward declaration');
  end;
  142:begin
   Write('Too less arguments then at forward declaration');
  end;
  143:begin
   Write('Already forward declared');
  end;
  144:begin
   Write('No definition for forward declared');
  end;
  145:begin
   Write('Internal negative retn');
  end;
  146:begin
   Write('Binary is too big');
  end;
  147:begin
   Write('String is too long');
  end;
  148:begin
   Write('Too many cases');
  end;
  149:begin
   Write('Too large code');
  end;
  150:begin
   Write('Incompatible types');
  end;
 end;
 WriteLn(' at line ',CurrentLine:1,' at column ',CurrentColumn:1);
 Halt;
end;

procedure ReadChar;
begin
 if not EOF then begin
  read(CurrentChar);
  CurrentColumn:=CurrentColumn+1;
  if CurrentChar=#10 then begin
   CurrentLine:=CurrentLine+1;
   CurrentColumn:=0;
  end;
 end else begin
  CurrentChar:=#0;
 end;
end;

function ReadNumber:integer;
var Num:integer;
begin
 Num:=0;
 if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
  while ('0'<=CurrentChar) and (CurrentChar<='9') do begin
   Num:=(Num*10)+(ord(CurrentChar)-ord('0'));
   ReadChar;
  end;
 end else if CurrentChar='$' then begin
  ReadChar;
  while (('0'<=CurrentChar) and (CurrentChar<='9')) or
        (('a'<=CurrentChar) and (CurrentChar<='f')) or
        (('A'<=CurrentChar) and (CurrentChar<='F')) do begin
   if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('0'));
   end else if ('a'<=CurrentChar) and (CurrentChar<='f') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('a')+10);
   end else if ('A'<=CurrentChar) and (CurrentChar<='F') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('A')+10);
   end;
   ReadChar;
  end;
 end;
 ReadNumber:=Num;
end;

procedure GetSymbol;
var k,s:integer;
    StrEnd,InStr:boolean;
    LastChar:char;
begin
 while (CurrentChar>#0) and (CurrentChar<=' ') do begin
  ReadChar;
 end;
 if (('a'<=CurrentChar) and (CurrentChar<='z')) or (('A'<=CurrentChar) and (CurrentChar<='Z')) then begin
  k:=0;
  while ((('a'<=CurrentChar) and (CurrentChar<='z')) or (('A'<=CurrentChar) and (CurrentChar<='Z')) or (('0'<=CurrentChar) and (CurrentChar<='9'))) or (CurrentChar='_') do begin
   if k<>MaximalAlfa then begin
    k:=k+1;
    if ('a'<=CurrentChar) and (CurrentChar<='z') then begin
     CurrentChar:=chr(ord(CurrentChar)-32);
    end;
    CurrentIdentifer[k]:=CurrentChar;
   end;
   ReadChar;
  end;
  while k<>MaximalAlfa do begin
   k:=k+1;
   CurrentIdentifer[k]:=' ';
  end;
  CurrentSymbol:=TokIdent;
  s:=SymBEGIN;
  while s<=SymPROC do begin
   if StringCompare(Keywords[s],CurrentIdentifer) then begin
    CurrentSymbol:=s;
   end;
   s:=s+1;
  end;
 end else if (('0'<=CurrentChar) and (CurrentChar<='9')) or (CurrentChar='$') then begin
  CurrentSymbol:=TokNumber;
  CurrentNumber:=ReadNumber;
 end else if CurrentChar=':' then begin
  ReadChar;
  if CurrentChar='=' then begin
   ReadChar;
   CurrentSymbol:=TokAssign;
  end else begin
   CurrentSymbol:=TokColon;
  end;
 end else if CurrentChar='>' then begin
  ReadChar;
  if CurrentChar='=' then begin
   ReadChar;
   CurrentSymbol:=TokGEq;
  end else begin
   CurrentSymbol:=TokGtr;
  end;
 end else if CurrentChar='<' then begin
  ReadChar;
  if CurrentChar='=' then begin
   ReadChar;
   CurrentSymbol:=TokLEq;
  end else if CurrentChar='>' then begin
   ReadChar;
   CurrentSymbol:=TokNEq;
  end else begin
   CurrentSymbol:=TokLss;
  end;
 end else if CurrentChar='.' then begin
  ReadChar;
  if CurrentChar='.' then begin
   ReadChar;
   CurrentSymbol:=TokColon;
  end else begin
   CurrentSymbol:=TokPeriod
  end;
 end else if (CurrentChar='''') or (CurrentChar='#') then begin
  CurrentStringLength:=0;
  StrEnd:=false;
  InStr:=false;
  CurrentSymbol:=TokStrC;
  while not StrEnd do begin
   if InStr then begin
    if CurrentChar='''' then begin
     ReadChar;
     if CurrentChar='''' then begin
      if CurrentStringLength=MaximalStringLength then begin
       Error(147);
      end;
      CurrentStringLength:=CurrentStringLength+1;
      CurrentString[CurrentStringLength]:=CurrentChar;
      ReadChar;
     end else begin
      InStr:=false;
     end;
    end else if (CurrentChar=#13) or (CurrentChar=#10) then begin
     Error(100);
     StrEnd:=true;
    end else begin
      if CurrentStringLength=MaximalStringLength then begin
       Error(147);
      end;
     CurrentStringLength:=CurrentStringLength+1;
     CurrentString[CurrentStringLength]:=CurrentChar;
     ReadChar;
    end;
   end else begin
    if CurrentChar='''' then begin
     InStr:=true;
     ReadChar;
    end else if CurrentChar='#' then begin
     ReadChar;
     if CurrentStringLength=MaximalStringLength then begin
      Error(147);
     end;
     CurrentStringLength:=CurrentStringLength+1;
     CurrentString[CurrentStringLength]:=chr(ReadNumber);
    end else begin
     StrEnd:=true;
    end;
   end;
  end;
  if CurrentStringLength=0 then begin
   Error(101);
  end;
 end else if CurrentChar='+' then begin
  ReadChar;
  CurrentSymbol:=TokPlus;
 end else if CurrentChar='-' then begin
  ReadChar;
  CurrentSymbol:=TokMinus;
 end else if CurrentChar='*' then begin
  ReadChar;
  CurrentSymbol:=TokMul;
 end else if CurrentChar='(' then begin
  ReadChar;
  if CurrentChar='*' then begin
   ReadChar;
   LastChar:='-';
   while (CurrentChar<>#0) and not ((CurrentChar=')') and (LastChar='*')) do begin
    LastChar:=CurrentChar;
    ReadChar;
   end;
   ReadChar;
   GetSymbol;
  end else begin
   CurrentSymbol:=TokLParent;
  end;
 end else if CurrentChar=')' then begin
  ReadChar;
  CurrentSymbol:=TokRParent;
 end else if CurrentChar='[' then begin
  ReadChar;
  CurrentSymbol:=TokLBracket;
 end else if CurrentChar=']' then begin
  ReadChar;
  CurrentSymbol:=TokRBracket;
 end else if CurrentChar='=' then begin
  ReadChar;
  CurrentSymbol:=TokEql;
 end else if CurrentChar=',' then begin
  ReadChar;
  CurrentSymbol:=TokComma;
 end else if CurrentChar=';' then begin
  ReadChar;
  CurrentSymbol:=TokSemi;
 end else if CurrentChar='{' then begin
  while (CurrentChar<>'}') and (CurrentChar<>#0) do begin
   ReadChar;
  end;
  ReadChar;
  GetSymbol;
 end else if CurrentChar='/' then begin
  ReadChar;
  if CurrentChar='/' then begin
   repeat
    ReadChar;
   until (CurrentChar=#10) or (CurrentChar=#0);
   GetSymbol;
  end else begin
   Error(102);
  end;
 end else begin
  Error(102);
 end;
end;

procedure Check(s:integer);
begin
 if CurrentSymbol<>s then begin
  Error(s);
 end;
end;

procedure Expect(s:integer);
begin
 Check(s);
 GetSymbol;
end;

procedure EnterSymbol(CurrentIdentifer:TAlfa;k,t:integer);
var j:integer;
begin
 if IdentifierPosition=MaximalIdentifiers then begin
  Error(103);
 end;
 IdentifierPosition:=IdentifierPosition+1;
 Identifiers[0].Name:=CurrentIdentifer;
 j:=SymbolNameList[CurrentLevel];
 while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do begin
  j:=Identifiers[j].Link;
 end;
 if j<>0 then begin
  if Identifiers[j].Kind<>IdFUNC then begin
   Error(104);
  end;
  if (Code[Identifiers[j].FunctionAddress]<>OPJmp) or (Code[Identifiers[j].FunctionAddress+1]>0) then begin
   Error(105);
  end;
  Identifiers[j].Name[1]:='$';
  Code[Identifiers[j].FunctionAddress+1]:=CodePosition;
  FunctionDeclarationIndex:=j;
 end;
 Identifiers[IdentifierPosition].Name:=CurrentIdentifer;
 Identifiers[IdentifierPosition].Link:=SymbolNameList[CurrentLevel];
 Identifiers[IdentifierPosition].TypeDefinition:=t;
 Identifiers[IdentifierPosition].Kind:=k;
 SymbolNameList[CurrentLevel]:=IdentifierPosition;
end;

function Position:integer;
var i,j:integer;
begin
 Identifiers[0].Name:=CurrentIdentifer;
 i:=CurrentLevel;
 repeat
  j:=SymbolNameList[i];
  while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do begin
   j:=Identifiers[j].Link;
  end;
  i:=i-1;
 until (i<-1) or (j<>0);
 if j=0 then begin
  Error(106);
 end;
 Position:=j;
end;

procedure EmitCode(Value:integer);
begin
 if CodePosition>MaximalCodeSize then begin
  Error(149);
 end;
 Code[CodePosition]:=Value;
 CodePosition:=CodePosition+1;
end;

procedure EmitOpcode(Opcode,a:integer);
begin
 case Opcode of
  OPDupl,OPEOF,OPEOL,OPLdC,OPLdA,OPLdLA,OPLdL,OPLdG:begin
   StackPosition:=StackPosition-4;
  end;
  OPNeg,OPDiv2,OPRem2,OPSwap,OPLoad,OPHalt,OPWrL,OPRdL,OpAddC,OPMulC,
  OPJmp,OPCall,OPExit:begin
  end;
  OPAdd,OPMul,OPDivD,OPRemD,OPEqlI,OPNEqI,OPLssI,OPLeqI,OPGtrI,OPGEqI,OPAndB,
  OPOrB,OPWrC,OPRdI,OPRdC,OPStL,OPStG,OPJZ:begin
   StackPosition:=StackPosition+4;
  end;
  OPStore,OPWrI,OPMove:begin
   StackPosition:=StackPosition+8;
  end;
  OPCopy:begin
   StackPosition:=StackPosition-(a-4);
  end;
  OPAdjS:begin
   StackPosition:=StackPosition+a;
  end;
 end;
 if not ((((Opcode=OPAddC) or (Opcode=OPAdjS)) and (a=0)) or ((Opcode=OPMulC) and (a=1))) then begin
  if IsLabeled then begin
   Code[CodePosition]:=Opcode;
   CodePosition:=CodePosition+1;
   if Opcode>=OPLdC then begin
    Code[CodePosition]:=a;
    CodePosition:=CodePosition+1;
   end;
   IsLabeled:=false;
  end else if (LastOpcode=OPLdC) and (Opcode=OPAdd) then begin
   Code[CodePosition-2]:=OPAddC;
  end else if (LastOpcode=OPLdC) and (Opcode=OPMul) then begin
   Code[CodePosition-2]:=OPMulC;
  end else if (LastOpcode=OPLdC) and (Opcode=OPNeg) then begin
   Code[CodePosition-1]:=-Code[CodePosition-1];
   Opcode:=LastOpcode;
  end else if (LastOpcode=OPLdC) and (Code[CodePosition-1]=2) and (Opcode=OPDivD) then begin
   Code[CodePosition-2]:=OPDiv2;
   CodePosition:=CodePosition-1;
  end else if (LastOpcode=OPLdC) and (Code[CodePosition-1]=2) and (Opcode=OPRemD) then begin
   Code[CodePosition-2]:=OPRem2;
   CodePosition:=CodePosition-1;
  end else if (LastOpcode=OPLdA) and (Opcode=OPStore) then begin
   Code[CodePosition-2]:=OPStG;
  end else if (LastOpcode=OPLdA) and (Opcode=OPLoad) then begin
   Code[CodePosition-2]:=OPLdG;
  end else if (LastOpcode=OPLdLA) and (Opcode=OPStore) then begin
   Code[CodePosition-2]:=OPStL;
  end else if (LastOpcode=OPLdLA) and (Opcode=OPLoad) then begin
   Code[CodePosition-2]:=OPLdL;
  end else begin
   EmitCode(Opcode);
   if Opcode>=OPLdC then begin
    EmitCode(a);
   end;
  end;
  LastOpcode:=Opcode;
 end;
end;

procedure EmitOpcode2(Opcode:integer);
begin
 EmitOpcode(Opcode,0);
end;

function CodeLabel:integer;
begin
 CodeLabel:=CodePosition;
 IsLabeled:=true;
end;

procedure EmitAddress(Level,Address:integer);
begin
 if Level=0 then begin
  EmitOpcode(OPLdA,Address);
 end else if Level=CurrentLevel then begin
  EmitOpcode(OPLdLA,Address-StackPosition);
 end else begin
  EmitOpcode(OPLdL,-StackPosition);
  while Level+1<>CurrentLevel do begin
   EmitOpcode2(OPLoad);
   Level:=Level+1;
  end;
  EmitOpcode(OPAddC,Address);
 end;
end;

procedure EmitAddressVar(IdentifierIndex:integer);
begin
 EmitAddress(Identifiers[IdentifierIndex].VariableLevel,Identifiers[IdentifierIndex].VariableAddress);
 if Identifiers[IdentifierIndex].ReferencedParameter then begin
  EmitOpcode2(OPLoad);
 end;
end;

procedure MustBe(x,y:integer);
begin
 if x<>y then begin
  if (Types[x].Kind=KindARRAY) and (Types[y].Kind=KindARRAY) and (Types[x].StartIndex=Types[y].StartIndex) and (Types[x].EndIndex=Types[y].EndIndex) then begin
   MustBe(Types[x].SubType,Types[y].SubType);
  end else begin
   Error(107);
  end;
 end;
end;

procedure Expression(var x:integer); forward;

procedure Selector(var t,IdentifierIndex:integer);
var j,x:integer;
begin
 t:=Identifiers[IdentifierIndex].TypeDefinition;
 GetSymbol;
 if (CurrentSymbol=TokPeriod) or (CurrentSymbol=TokLBracket) then begin
  EmitAddressVar(IdentifierIndex);
  IdentifierIndex:=0;
  while (CurrentSymbol=TokPeriod) or (CurrentSymbol=TokLBracket) do begin
   case CurrentSymbol of
    TokPeriod:begin
     if Types[t].Kind<>KindRECORD then begin
      Error(108);
     end;
     GetSymbol;
     Check(TokIdent);
     j:=Types[t].Fields;
     Identifiers[0].Name:=CurrentIdentifer;
     while not StringCompare(Identifiers[j].Name,CurrentIdentifer) do begin
      j:=Identifiers[j].Link;
     end;
     if j=0 then begin
      Error(109);
     end;
     EmitOpcode(OPAddC,Identifiers[j].Offset);
     t:=Identifiers[j].TypeDefinition;
     GetSymbol;
    end;
    TokLBracket:begin
     repeat
      if Types[t].Kind<>KindARRAY then begin
       Error(110);
      end;
      GetSymbol;
      Expression(x);
      MustBe(TypeINT,x);
      EmitOpcode(OPAddC,-Types[t].StartIndex);
      t:=Types[t].SubType;
      EmitOpcode(OPMulC,Types[t].Size);
      EmitOpcode2(OPAdd);
     until CurrentSymbol<>TokComma;
     Expect(TokRBracket)
    end;
   end;
  end;
 end;
end;

procedure VarPar(var t:integer);
var j:integer;
begin
 Check(TokIdent);
 j:=Position;
 Selector(t,j);
 if j<>0 then begin
  EmitAddressVar(j);
 end;
end;

procedure InternalFunction(n:integer);
var x:integer;
begin
 case n of
  FunCHR:begin
   Expect(TokLParent);
   Expression(x);
   MustBe(TypeINT,x);
   Expect(TokRParent)
  end;
  FunORD:begin
   Expect(TokLParent);
   Expression(x);
   if x<>TypeBOOL then begin
    MustBe(TypeCHAR,x);
   end;
   Expect(TokRParent);
  end;
  FunWRITE,FunWRITELN:begin
   if n=FunWRITE then begin
    Check(TokLParent);
   end;
   if CurrentSymbol=TokLParent then begin
    repeat
     GetSymbol;
     if CurrentSymbol=TokStrC then begin
      x:=1;
      while x<=CurrentStringLength do begin
       EmitOpcode(OPLdC,ord(CurrentString[x]));
       EmitOpcode2(OPWrC);
       x:=x+1;
      end;
      GetSymbol;
     end else begin
      Expression(x);
      if CurrentSymbol=TokColon then begin
       MustBe(TypeINT,x);
       GetSymbol;
       Expression(x);
       MustBe(TypeINT,x);
       EmitOpcode2(OPWrI);
      end else if x=TypeINT then begin
       EmitOpcode(OPLdC,1);
       EmitOpcode2(OPWrI);
      end else if x=TypeCHAR then begin
       EmitOpcode2(OPWrC);
      end else begin
       Error(111);
      end;
     end;
    until CurrentSymbol<>TokComma;
    Expect(TokRParent)
   end;
   if n=FunWRITELN then begin
    EmitOpcode2(OPWrL);
   end; 
  end;
  FunREAD,FunREADLN:begin
   if n=FunREAD then begin
    Check(TokLParent);
   end;
   if CurrentSymbol=TokLParent then begin
    repeat
     GetSymbol;
     VarPar(x);
     if x=TypeINT then begin
      EmitOpcode2(OPRdI);
     end else if x=TypeCHAR then begin
      EmitOpcode2(OPRdC);
     end else begin
      Error(112);
     end;
    until CurrentSymbol<>TokComma;
    Expect(TokRParent);
   end;
   if n=FunREADLN then begin
    EmitOpcode2(OPRdL);
   end;
  end;
  FunEOF:begin
   EmitOpcode2(OPEOF);
  end;
  FunEOFLN:begin
   EmitOpcode2(OPEOL);
  end;
 end;
end;

procedure FunctionCall(i:integer);
var OldStackPosition,p,x:integer;
begin
 GetSymbol;
 if Identifiers[i].FunctionLevel<0 then begin
  InternalFunction(Identifiers[i].FunctionAddress);
 end else begin
  if Identifiers[i].TypeDefinition<>0 then begin
   EmitOpcode(OPLdC,0);
  end;
  p:=i;
  OldStackPosition:=StackPosition;
  if CurrentSymbol=TokLParent then begin
   repeat
    GetSymbol;
    if p=Identifiers[i].LastParameter then begin
     Error(113);
    end;
    p:=p+1;
    if Identifiers[p].ReferencedParameter then begin
     VarPar(x);
    end else begin
     Expression(x);
     if Types[x].Kind<>KindSIMPLE then begin
      EmitOpcode(OPCopy,Types[x].Size);
     end;
    end;
    if x=TypeSTR then begin
     if Identifiers[p].ReferencedParameter then begin
      Error(114);
     end;
     if Types[Identifiers[p].TypeDefinition].Kind<>KindARRAY then begin
      Error(115);
     end;
     if Types[Identifiers[p].TypeDefinition].SubType<>TypeCHAR then begin
      Error(116);
     end;
     if ((Types[Identifiers[p].TypeDefinition].EndIndex-Types[Identifiers[p].TypeDefinition].StartIndex)+1)<>CurrentStringLength then begin
      Error(117);
     end;
    end else begin
     MustBe(Identifiers[p].TypeDefinition,x);
    end;
   until CurrentSymbol<>TokComma;
   Expect(TokRParent);
  end;
  if p<>Identifiers[i].LastParameter then begin
   Error(118);
  end;
  if Identifiers[i].FunctionLevel<>0 then begin
   EmitAddress(Identifiers[i].FunctionLevel,0);
  end;
  EmitOpcode(OPCall,Identifiers[i].FunctionAddress);
  StackPosition:=OldStackPosition;
 end;
end;

procedure Factor(var t:integer);
var i:integer;
begin
 if CurrentSymbol=TokIdent then begin
  i:=Position;
  t:=Identifiers[i].TypeDefinition;
  case Identifiers[i].Kind of
   IdCONST:begin
    GetSymbol;
    EmitOpcode(OPLdC,Identifiers[i].Value);
   end;
   IdVAR:begin
    Selector(t,i);
    if i<>0 then begin
     EmitAddressVar(i);
    end;
    if Types[t].Kind=KindSIMPLE then begin
     EmitOpcode2(OPLoad);
    end;
   end;
   IdFUNC:begin
    if t=0 then begin
     Error(119);
    end else begin
     FunctionCall(i);
    end;
   end;
   IdTYPE:begin
    Error(120);
   end;
  end;
 end else if CurrentSymbol=TokNumber then begin
  EmitOpcode(OPLdC,CurrentNumber);
  t:=TypeINT;
  GetSymbol;
 end  else if CurrentSymbol=TokStrC then begin
  i:=CurrentStringLength;
  while i>=1 do begin
   EmitOpcode(OPLdC,ord(CurrentString[i]));
   i:=i-1;
  end;
  t:=TypeCHAR;
  if CurrentStringLength<>1 then begin
   t:=TypeSTR;
  end;
  GetSymbol;
 end else if CurrentSymbol=TokLParent then begin
  GetSymbol;
  Expression(t);
  Expect(TokRParent);
 end else if CurrentSymbol=SymNOT then begin
  GetSymbol;
  Factor(t);
  MustBe(TypeBOOL,t);
  EmitOpcode2(OPNeg);
  EmitOpcode(OPAddC,1);
 end else begin
  Error(121);
 end;
end;

procedure Term(var x:integer);
var y:integer;
begin
 Factor(x);
 while (CurrentSymbol=SymAND) or (CurrentSymbol=TokMul) or (CurrentSymbol=SymDIV) or (CurrentSymbol=SymMOD) do begin
  if CurrentSymbol=SymAND then begin
   MustBe(TypeBOOL,x);
  end else begin
   MustBe(TypeINT,x);
  end;
  case CurrentSymbol of
   TokMul:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPMul);
   end;
   SymDIV:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPDivD);
   end;
   SymMOD:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPRemD);
   end;
   SymAND:begin
    GetSymbol;
    Factor(y);
    EmitOpcode2(OPAndB);
   end;
  end;
  MustBe(x,y);
 end;
end;

procedure SimpleExpression(var x:integer);
var y:integer;
begin
 if CurrentSymbol=TokPlus then begin
  GetSymbol;
  Term(x);
  MustBe(TypeINT,x);
 end else if CurrentSymbol=TokMinus then begin
  GetSymbol;
  Term(x);
  MustBe(TypeINT,x);
  EmitOpcode2(OPNeg);
 end else begin
  Term(x);
 end;
 while (CurrentSymbol=SymOR) or (CurrentSymbol=TokPlus) or (CurrentSymbol=TokMinus) do begin
  if CurrentSymbol=SymOR then begin
   MustBe(TypeBOOL,x);
  end else begin
   MustBe(TypeINT,x);
  end;
  case CurrentSymbol of
   TokPlus:begin
    GetSymbol;
    Term(y);
    EmitOpcode2(OPAdd);
   end;
   TokMinus:begin
    GetSymbol;
    Term(y);
    EmitOpcode2(OPNeg);
    EmitOpcode2(OPAdd);
   end;
   SymOR:begin
    GetSymbol;
    Term(y);
    EmitOpcode2(OPOrB);
   end;
  end;
  MustBe(x,y);
 end;
end;

procedure Expression(var x:integer);
var o,y:integer;
begin
 SimpleExpression(x);
 if (CurrentSymbol=TokEql) or (CurrentSymbol=TokNEq) or (CurrentSymbol=TokLss) or (CurrentSymbol=TokLEq) or (CurrentSymbol=TokGtr) or (CurrentSymbol=TokGEq) then begin
  if (x=TypeSTR) or (Types[x].Kind<>KindSIMPLE) then begin
   Error(150);
  end;
  o:=CurrentSymbol;
  GetSymbol;
  SimpleExpression(y);
  MustBe(x,y);
  case o of
   TokEql:begin
    EmitOpcode2(OPEqlI);
   end;
   TokNEq:begin
    EmitOpcode2(OPNEqI);
   end;
   TokLss:begin
    EmitOpcode2(OPLssI);
   end;
   TokLEq:begin
    EmitOpcode2(OPLeqI);
   end;
   TokGtr:begin
    EmitOpcode2(OPGtrI);
   end;
   TokGEq:begin
    EmitOpcode2(OPGEqI);
   end;
  end;
  x:=TypeBOOL;
 end;
end;

procedure Statement;
var L:array[1..MaximalCases] of integer;
    m,n,i,j,t,x,r,OldStackPosition:integer;
begin
 if CurrentSymbol=TokIdent then begin
  i:=Position;
  case Identifiers[i].Kind of
   IdVAR:begin
    Selector(t,i);
    Expect(TokAssign);
    Expression(x);
    MustBe(t,x);
    if i=0 then begin
     EmitOpcode2(OPSwap);
    end else begin
     EmitAddressVar(i);
    end;
    if Types[t].Kind=KindSIMPLE then begin
     EmitOpcode2(OPStore);
    end else begin
     EmitOpcode(OPMove,Types[t].Size);
    end;
   end;
   IdFUNC:begin
    if Identifiers[i].TypeDefinition=0 then begin
     FunctionCall(i);
    end else begin
     if not Identifiers[i].Inside then begin
      Error(122);
     end;
     GetSymbol;
     Expect(TokAssign);
     Expression(x);
     MustBe(Identifiers[i].TypeDefinition,x);
     EmitAddress(Identifiers[i].FunctionLevel+1,Identifiers[i].ReturnAddress);
     EmitOpcode2(OPStore);
    end;
   end;
   IdCONST,IdFIELD,IdTYPE:Error(123);
  end;
 end else if CurrentSymbol=SymIF then begin
  GetSymbol;
  Expression(t);
  MustBe(TypeBOOL,t);
  Expect(SymTHEN);
  i:=CodeLabel;
  EmitOpcode(OPJZ,0);
  Statement;
  if CurrentSymbol=SymELSE then begin
   GetSymbol;
   j:=CodeLabel;
   EmitOpcode(OPJmp,0);
   Code[i+1]:=CodeLabel;
   i:=j;
   Statement;
  end;
  Code[i+1]:=CodeLabel;
 end else if CurrentSymbol=SymCASE then begin
  GetSymbol;
  Expression(t);
  MustBe(TypeINT,t);
  Expect(SymOF);
  j:=0;
  m:=0;
  repeat
   if j<>0 then begin
    Code[j+1]:=CodeLabel;
   end;
   n:=m;
   repeat
    if n<>m then begin
     GetSymbol;
    end;
    EmitOpcode2(OPDupl);
    if CurrentSymbol=TokIdent then begin
     i:=Position;
     if Identifiers[i].Kind<>IdCONST then begin
      Error(124);
     end;
     EmitOpcode(OPLdC,Identifiers[i].Value);
    end else if CurrentSymbol=TokNumber then begin
     EmitOpcode(OPLdC,CurrentNumber);
    end else if (CurrentSymbol=TokStrC) and (CurrentStringLength=1) then begin
     EmitOpcode(OPLdC,ord(CurrentString[1]));
    end else begin
     Error(125);
    end;
    EmitOpcode2(OPNEqI);
    if n=MaximalCases then begin
     Error(148);
    end;
    n:=n+1;
    L[n]:=CodeLabel;
    EmitOpcode(OPJZ,0);
    GetSymbol;
   until CurrentSymbol<>TokComma;
   if CurrentSymbol<>TokColon then begin
    Error(126);
   end;
   j:=CodeLabel;
   EmitOpcode(OPJmp,0);
   repeat
    Code[L[n]+1]:=CodeLabel;
    n:=n-1;
   until n=m;
   GetSymbol;
   Statement;
   m:=m+1;
   L[m]:=CodeLabel;
   EmitOpcode(OPJmp,0);
   if CurrentSymbol=TokSemi then begin
    GetSymbol;
   end;
  until CurrentSymbol=SymEND;
  Code[j+1]:=CodeLabel;
  repeat
   Code[L[m]+1]:=CodeLabel;
   m:=m-1;
  until m=0;
  EmitOpcode(OPAdjS,4);
  GetSymbol;
 end else if CurrentSymbol=SymFOR then begin
  GetSymbol;
  if CurrentSymbol=TokIdent then begin
   OldStackPosition:=StackPosition;

   i:=Position;
   if Identifiers[i].Kind<>IdVAR then begin
    Error(127);
   end;
   Selector(t,i);
   Expect(TokAssign);
   Expression(x);
   MustBe(t,x);
   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   if Types[t].Kind<>KindSIMPLE then begin
    Error(128);
   end;
   EmitOpcode2(OPStore);

   r:=1;
   if CurrentSymbol=SymTO then begin
    Expect(SymTO);
   end else if CurrentSymbol=SymDOWNTO then begin
    Expect(SymDOWNTO);
    r:=-1;
   end else begin
    Error(129);
   end;

   j:=CodeLabel;
   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   EmitOpcode2(OPLoad);
   Expression(x);
   MustBe(t,x);
   if r>0 then begin
    EmitOpcode2(OPLeqI);
   end else begin
    EmitOpcode2(OPGeqI);
   end;
   n:=CodeLabel;
   EmitOpcode(OPJZ,0);

   Expect(SymDO);

   Statement;

   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   EmitOpcode2(OPLoad);

   EmitOpcode(OPAddC,r);

   if i=0 then begin
    EmitOpcode2(OPSwap);
   end else begin
    EmitAddressVar(i);
   end;
   EmitOpcode2(OPStore);

   EmitOpcode(OPJmp,j);
   Code[n+1]:=CodeLabel;

   EmitOpcode(OPAdjS,OldStackPosition-StackPosition);

  end else begin
   Expect(TokIdent);
  end;
 end else if CurrentSymbol=SymWHILE then begin
  GetSymbol;
  i:=CodeLabel;
  Expression(t);
  MustBe(TypeBOOL,t);
  Expect(SymDO);
  j:=CodeLabel;
  EmitOpcode(OPJZ,0);
  Statement;
  EmitOpcode(OPJmp,i);
  Code[j+1]:=CodeLabel;
 end else if CurrentSymbol=SymREPEAT then begin
  i:=CodeLabel;
  repeat
   GetSymbol;
   Statement;
  until CurrentSymbol<>TokSemi;
  Expect(SymUNTIL);
  Expression(t);
  MustBe(TypeBOOL,t);
  EmitOpcode(OPJZ,i);
 end else if CurrentSymbol=SymBEGIN then begin
  repeat
   GetSymbol;
   Statement;
  until CurrentSymbol<>TokSemi;
  Expect(SymEND);
 end else if CurrentSymbol=SymHALT then begin
  EmitOpcode2(OPHalt);
  GetSymbol;
 end;
end;

procedure Block(L:integer); forward;

procedure Constant(var c,t:integer);
var i,s:integer;
begin
 if (CurrentSymbol=TokStrC) and (CurrentStringLength=1) then begin
  c:=ord(CurrentString[1]);
  t:=TypeCHAR;
 end else begin
  if CurrentSymbol=TokPlus then begin
   GetSymbol;
   s:=1;
  end  else if CurrentSymbol=TokMinus then begin
   GetSymbol;
   s:=-1;
  end else begin
   s:=0;
  end;
  if CurrentSymbol=TokIdent then begin
   i:=Position;
   if Identifiers[i].Kind<>IdCONST then begin
    Error(130);
   end;
   c:=Identifiers[i].Value;
   t:=Identifiers[i].TypeDefinition;
  end else if CurrentSymbol=TokNumber then begin
   c:=CurrentNumber;
   t:=TypeINT;
  end else begin
   Error(131);
  end;
  if s<>0 then begin
   MustBe(t,TypeINT);
   c:=c*s;
  end;
 end;
 GetSymbol;
end;

procedure ConstDeclaration;
var a:TAlfa;
    t,c:integer;
begin
 a:=CurrentIdentifer;
 GetSymbol;
 Expect(TokEql);
 Constant(c,t);
 Expect(TokSemi);
 EnterSymbol(A,IdCONST,t);
 Identifiers[IdentifierPosition].Value:=c;
end;

procedure TypeDefinition(var t:integer); forward;

procedure ArrayType(var t:integer);
var x:integer;
begin
 Types[t].Kind:=KindARRAY;
 GetSymbol;
 Constant(Types[t].StartIndex,x);
 MustBe(TypeINT,x);
 Expect(TokColon);
 Constant(Types[t].EndIndex,x);
 MustBe(TypeINT,x);
 if Types[t].StartIndex>Types[t].EndIndex then begin
  Error(132);
 end;
 if CurrentSymbol=TokComma then begin
  ArrayType(Types[t].SubType);
 end else begin
  Expect(TokRBracket);
  Expect(SymOF);
  TypeDefinition(Types[t].SubType);
 end;
 Types[t].Size:=(Types[t].EndIndex-Types[t].StartIndex+1)*Types[Types[t].SubType].Size;
end;

procedure TypeDefinition(var t:integer);
var i,j,Size,FieldType:integer;
begin
 if CurrentSymbol=SymPACKED then begin
  GetSymbol;
 end; 
 if CurrentSymbol=TokIdent then begin
  i:=Position;
  if Identifiers[i].Kind<>IdTYPE then begin
   Error(133);
  end;
  t:=Identifiers[i].TypeDefinition;
  GetSymbol;
 end else begin
  if TypePosition=MaximalTypes then begin
   Error(134);
  end;
  TypePosition:=TypePosition+1;
  t:=TypePosition;
  if CurrentSymbol=SymARRAY then begin
   GetSymbol;
   Check(TokLBracket);
   ArrayType(t);
  end else begin
   Expect(SymRECORD);
   if CurrentLevel=MaximalList then begin
    Error(135);
   end;
   CurrentLevel:=CurrentLevel+1;
   SymbolNameList[CurrentLevel]:=0;
   Check(TokIdent);
   Size:=0;
   repeat
    EnterSymbol(CurrentIdentifer,IdFIELD,0);
    i:=IdentifierPosition;
    GetSymbol;
    while CurrentSymbol=TokComma do begin
     GetSymbol;
     Check(TokIdent);
     EnterSymbol(CurrentIdentifer,IdFIELD,0);
     GetSymbol;
    end;
    j:=IdentifierPosition;
    Expect(TokColon);
    TypeDefinition(FieldType);
    repeat
     Identifiers[i].TypeDefinition:=FieldType;
     Identifiers[i].Offset:=Size;
     Size:=Size+Types[FieldType].Size;
     i:=i+1;
    until i>j;
    if CurrentSymbol=TokSemi then begin
     GetSymbol;
    end else begin
     Check(SymEND);
    end;
   until CurrentSymbol<>TokIdent;
   Types[t].Size:=Size;
   Types[t].Kind:=KindRECORD;
   Types[t].Fields:=SymbolNameList[CurrentLevel];
   CurrentLevel:=CurrentLevel-1;
   Expect(SymEND);
  end;
 end;
end;

procedure TypeDeclaration;
var a:TAlfa;
    t:integer;
begin
 a:=CurrentIdentifer;
 GetSymbol;
 Expect(TokEql);
 TypeDefinition(t);
 Expect(TokSemi);
 EnterSymbol(a,IdTYPE,t);
end;

procedure VarDeclaration;
var p,q,t:integer;
begin
 EnterSymbol(CurrentIdentifer,IdVAR,0);
 p:=IdentifierPosition;
 GetSymbol;
 while CurrentSymbol=TokComma do begin
  GetSymbol;
  Check(TokIdent);
  EnterSymbol(CurrentIdentifer,IdVAR,0);
  GetSymbol;
 end;
 q:=IdentifierPosition;
 Expect(TokColon);
 TypeDefinition(t);
 Expect(TokSemi);
 repeat
  Identifiers[p].VariableLevel:=CurrentLevel;
  StackPosition:=StackPosition-Types[t].Size;
  Identifiers[p].TypeDefinition:=t;
  Identifiers[p].VariableAddress:=StackPosition;
  Identifiers[p].ReferencedParameter:=false;
  p:=p+1;
 until p>q;
end;

procedure NewParameter(var p,LocalStackPosition:integer);
var r:boolean;
    t:integer;
begin
 if CurrentSymbol=SymVAR then begin
  r:=true;
  GetSymbol;
 end else begin
  r:=false;
 end;
 Check(TokIdent);
 p:=IdentifierPosition;
 EnterSymbol(CurrentIdentifer,IdVAR,0);
 GetSymbol;
 while CurrentSymbol=TokComma do begin
  GetSymbol;
  Check(TokIdent);
  EnterSymbol(CurrentIdentifer,IdVAR,0);
  GetSymbol;
 end;
 Expect(TokColon);
 Check(TokIdent);
 TypeDefinition(t);
 while p<IdentifierPosition do begin
  p:=p+1;
  Identifiers[p].TypeDefinition:=t;
  Identifiers[p].ReferencedParameter:=r;
  if r then begin
   LocalStackPosition:=LocalStackPosition+4;
  end else begin
   LocalStackPosition:=LocalStackPosition+Types[t].Size;
  end;
 end;
end;

procedure FunctionDeclaration(IsFunction:boolean);
var f,p,LocalStackPosition,P1,P2,OldStackPosition:integer;
begin
 GetSymbol;
 Check(TokIdent);
 FunctionDeclarationIndex:=-1;
 EnterSymbol(CurrentIdentifer,IdFUNC,0);
 GetSymbol;
 f:=IdentifierPosition;
 Identifiers[f].FunctionLevel:=CurrentLevel;
 Identifiers[f].FunctionAddress:=CodeLabel;
 EmitOpcode(OPJmp,0);
 if CurrentLevel=MaximalList then begin
  Error(136);
 end;
 CurrentLevel:=CurrentLevel+1;
 SymbolNameList[CurrentLevel]:=0;
 LocalStackPosition:=4;
 OldStackPosition:=StackPosition;
 if CurrentSymbol=TokLParent then begin
  repeat
   GetSymbol;
   NewParameter(p,LocalStackPosition);
  until CurrentSymbol<>TokSemi;
  Expect(TokRParent);
 end;
 if CurrentLevel>1 then begin
  StackPosition:=-4;
 end else begin
  StackPosition:=0;
 end;
 Identifiers[f].ReturnAddress:=LocalStackPosition;
 p:=f;
 while p<IdentifierPosition do begin
  p:=p+1;
  if Identifiers[p].ReferencedParameter then begin
   LocalStackPosition:=LocalStackPosition-4;
  end else begin
   LocalStackPosition:=LocalStackPosition-Types[Identifiers[p].TypeDefinition].Size;
  end;
  Identifiers[p].VariableLevel:=CurrentLevel;
  Identifiers[p].VariableAddress:=LocalStackPosition;
 end;
 if IsFunction then begin
  Expect(TokColon);
  Check(TokIdent);
  TypeDefinition(Identifiers[f].TypeDefinition);
  if Types[Identifiers[f].TypeDefinition].Kind<>KindSIMPLE then begin
   Error(137);
  end;
 end;
 Expect(TokSemi);
 Identifiers[f].LastParameter:=IdentifierPosition;
 if CurrentSymbol<>SymFORWARD then begin
  if FunctionDeclarationIndex>=0 then begin
   P1:=FunctionDeclarationIndex+1;
   P2:=f+1;
   while P1<=Identifiers[FunctionDeclarationIndex].LastParameter do begin
    if P2>Identifiers[f].LastParameter then begin
     Error(138);
    end;
    if not StringCompare(Identifiers[P1].Name,Identifiers[P2].Name) then begin
     Error(139);
    end;
    if Identifiers[P1].TypeDefinition<>Identifiers[P2].TypeDefinition then begin
     Error(140);
    end;
    if Identifiers[P1].ReferencedParameter<>Identifiers[P2].ReferencedParameter then begin
     Error(141);
    end;
    P1:=P1+1;
    P2:=P2+1;
   end;
   if P2<=Identifiers[f].LastParameter then begin
    Error(142);
   end;
  end;
  Identifiers[f].Inside:=true;
  Block(Identifiers[f].FunctionAddress);
  Identifiers[f].Inside:=false;
  EmitOpcode(OPExit,Identifiers[f].ReturnAddress-StackPosition);
 end else begin
  if FunctionDeclarationIndex>=0 then begin
   Error(143);
  end;
  GetSymbol;
 end;
 CurrentLevel:=CurrentLevel-1;
 StackPosition:=OldStackPosition;
 Expect(TokSemi);
end;

procedure Block(L:integer);
var i,d,OldStackPosition,OldIdentPos:integer;
begin
 OldStackPosition:=StackPosition;
 OldIdentPos:=IdentifierPosition;
 while (CurrentSymbol=SymCONST) or (CurrentSymbol=SymTYPE) or (CurrentSymbol=SymVAR) or (CurrentSymbol=SymFUNC) or (CurrentSymbol=SymPROC) do begin
  if CurrentSymbol=SymCONST then begin
   GetSymbol;
   Check(TokIdent);
   while CurrentSymbol=TokIdent do begin
    ConstDeclaration;
   end;
  end else if CurrentSymbol=SymTYPE then begin
   GetSymbol;
   Check(TokIdent);
   while CurrentSymbol=TokIdent do begin
    TypeDeclaration;
   end;
  end else if CurrentSymbol=SymVAR then begin
   GetSymbol;
   Check(TokIdent);
   while CurrentSymbol=TokIdent do begin
    VarDeclaration;
   end;
  end else if (CurrentSymbol=SymFUNC) or (CurrentSymbol=SymPROC) then begin
   FunctionDeclaration(CurrentSymbol=SymFUNC);
  end;
 end;
 if L+1=CodeLabel then begin
  CodePosition:=CodePosition-1;
 end else begin
  Code[L+1]:=CodeLabel;
 end;
 if CurrentLevel=0 then begin
  EmitOpcode(OPAdjS,StackPosition);
 end else begin
  d:=StackPosition-OldStackPosition;
  StackPosition:=OldStackPosition;
  EmitOpcode(OPAdjS,d);
 end;
 Statement;
 if CurrentLevel<>0 then begin
  EmitOpcode(OPAdjS,OldStackPosition-StackPosition);
 end;
 i:=OldIdentPos+1;
 while i<=IdentifierPosition do begin
  if Identifiers[i].Kind=IdFUNC then begin
   if (Code[Identifiers[i].FunctionAddress]=OPJmp) and (Code[Identifiers[i].FunctionAddress+1]=0) then begin
    Error(144);
   end;
  end;
  i:=i+1;
 end;
 IdentifierPosition:=OldIdentPos;
end;

const OutputCodeDataMaximalSize=262144;

var OutputCodeData:array[1..OutputCodeDataMaximalSize] of char;
    OutputCodeDataSize:integer;

procedure EmitChar(c:char);
begin
 OutputCodeDataSize:=OutputCodeDataSize+1;
 if OutputCodeDataSize>OutputCodeDataMaximalSize then begin
  Error(146);
 end;
 OutputCodeData[OutputCodeDataSize]:=c;
end;

procedure EmitByte(B:integer);
begin
 EmitChar(chr(B));
end;

procedure EmitInt16(i:integer);
begin
 if i>=0 then begin
  EmitByte(i mod 256);
  EmitByte((i div 256) mod 256);
 end else begin
  i:=-(i+1);
  EmitByte(255-(i mod 256));
  EmitByte(255-((i div 256) mod 256));
 end;
end;

procedure EmitInt32(i:integer);
begin
 if i>=0 then begin
  EmitByte(i mod 256);
  EmitByte((i div 256) mod 256);
  EmitByte((i div 65536) mod 256);
  EmitByte(i div 16777216);
 end else begin
  i:=-(i+1);        
  EmitByte(255-(i mod 256));
  EmitByte(255-((i div 256) mod 256));
  EmitByte(255-((i div 65536) mod 256));
  EmitByte(255-(i div 16777216));
 end;
end;

function OutputCodeGetInt32(o:integer):integer;
begin
 if ord(OutputCodeData[o+3])<$80 then begin
  OutputCodeGetInt32:=ord(OutputCodeData[o])+(ord(OutputCodeData[o+1])*256)+(ord(OutputCodeData[o+2])*65536)+(ord(OutputCodeData[o+3])*16777216);
 end else begin
  OutputCodeGetInt32:=-(((255-ord(OutputCodeData[o]))+((255-ord(OutputCodeData[o+1]))*256)+((255-ord(OutputCodeData[o+2]))*65536)+((255-ord(OutputCodeData[o+3]))*16777216))+1);
 end;
end;

procedure OutputCodePutInt32(o,i:integer);
begin
 if i>=0 then begin
  OutputCodeData[o]:=chr(i mod 256);
  OutputCodeData[o+1]:=chr((i div 256) mod 256);
  OutputCodeData[o+2]:=chr((i div 65536) mod 256);
  OutputCodeData[o+3]:=chr(i div 16777216);
 end else begin
  i:=-(i+1);
  OutputCodeData[o]:=chr(255-(i mod 256));
  OutputCodeData[o+1]:=chr(255-((i div 256) mod 256));
  OutputCodeData[o+2]:=chr(255-((i div 65536) mod 256));
  OutputCodeData[o+3]:=chr(255-(i div 16777216));
 end;
end;

procedure WriteOutputCode;
var i:integer;
begin
 for i:=1 to OutputCodeDataSize do begin
  write(OutputCodeData[i]);
 end;
end;

type TOutputCodeString=array[1..255] of char;

procedure OutputCodeString(s:TOutputCodeString);
var i:integer;
begin
 for i:=1 to 255 do begin
  EmitChar(s[i]);
 end;
end;

procedure EmitStubCode;
begin
 OutputCodeDataSize:=0;
 OutputCodeString(#77#90#82#195#66#101#82#111#94#102#114#0#80#69#0#0#76#1#1#0#0#0#0#0#0#0#0#0#0#0#0#0#224#0#15#3#11#1#0#0#143#3#0#0#0#0#0#0#0#0#0#0#196#16#0#0#0#16#0#0#12#0#0#0#0#0#64#0#0#16#0#0#0#2#0#0#4#0#0#0#0#0#0#0#4#0#0#0#0#0#0#0#0#32#0#0#0#2#0#0#0#0#0#0#3#0#0#0#0#0#16#0#0#32#0#0#0#0#16#0#0#32#0#0#0#0#0#0#16#0#0#0#0#0#0#0#0#0#0#0#0#16#0#0#196#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OutputCodeString(#0#0#0#0#0#0#0#0#0#0#0#0#0#0#16#0#0#0#16#0#0#143#3#0#0#0#2#0#0#0#0#0#0#0#0#0#0#0#0#0#0#32#0#0#224#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0);
 OutputCodeString(#0#0#0#0#0#0#0#0#0#0#255#255#255#255#40#16#0#0#53#16#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#107#101#114#110#101#108#51#50#46#100#108#108#0#87#16#0#0#101#16#0#0#116#16#0#0#133#16#0#0#145#16#0#0#156#16#0#0#173#16#0#0#185#16#0#0#0#0#0#0#69#120#105#116#80#114#111#99#101#115#115#0#0#0#71#101#116#83#116#100#72#97#110#100#108#101#0#0#0#83#101#116#67#111#110#115#111#108#101#77#111#100#101#0#0#0#87#114#105#116#101#70#105#108#101#0#0#0#82#101#97#100#70#105#108#101#0#0#0#71#101#116#80#114#111#99#101#115#115#72#101#97#112#0#0#0#72#101#97#112#65#108#108#111#99#0#0#0#72#101#97#112#70#114#101#101#0#233#102#2#0#0#32#67#111#109#112#105#108#101#100#32#98#121#58#32#66#101#82#111#84#105#110#121#80#97#115#99#97#108#32#45#32#40#67#41#32#67#111#112#121#114#105#103#104#116#32#50#48#48#54#44#32#66);
 OutputCodeString(#101#110#106#97#109#105#110#32#39#66#101#82#111#39#32#82#111#115#115#101#97#117#120#32#144#141#64#0#86#141#116#36#8#96#106#0#104#21#17#64#0#106#1#86#255#53#43#19#64#0#255#21#65#16#64#0#97#94#194#4#0#144#141#64#0#144#141#64#0#144#141#64#0#86#139#92#36#8#139#68#36#12#131#248#0#125#10#247#216#75#106#45#232#187#255#255#255#51#201#80#83#133#192#116#12#65#187#10#0#0#0#51#210#247#251#235#240#133#201#15#148#210#10#202#91#88#43#217#131#251#0#126#12#81#106#32#232#143#255#255#255#75#117#246#89#141#185#57#17#64#0#81#190#10#0#0#0#51#210#247#254#141#90#48#136#31#79#226#239#89#106#0#104#21#17#64#0#81#104#58#17#64#0#255#53#43#19#64#0#255#21#65#16#64#0#94#194#8#0#106#13#232#78#255#255#255#106#10#232#71#255#255#255#195#144#144#141#64#0#96#106#0#104#212#17#64#0#106#1#104#211#17#64#0#255#53#39#19#64#0#255#21#69#16#64#0#133#192#15#148#208#8#5#190#18);
 OutputCodeString(#64#0#131#61#212#17#64#0#0#15#148#208#8#5#190#18#64#0#97#195#0#128#61#16#18#64#0#0#117#12#232#185#255#255#255#198#5#16#18#64#0#1#195#232#229#255#255#255#15#182#5#211#17#64#0#232#160#255#255#255#195#232#211#255#255#255#96#51#192#141#72#1#128#61#190#18#64#0#0#117#72#128#61#211#17#64#0#0#116#16#128#61#211#17#64#0#32#119#7#232#116#255#255#255#235#222#128#61#211#17#64#0#45#117#7#247#217#232#98#255#255#255#15#182#29#211#17#64#0#128#251#48#114#19#128#251#57#119#14#107#192#10#141#68#24#208#232#69#255#255#255#235#225#247#233#137#68#36#28#97#195#232#111#255#255#255#128#61#190#18#64#0#0#117#18#138#29#211#17#64#0#128#251#10#116#7#232#29#255#255#255#235#224#195#0#15#182#5#190#18#64#0#195#128#61#211#17#64#0#10#15#148#210#195#144#141#64#0#139#37#210#18#64#0#255#53#35#19#64#0#104#0#0#0#0#255#53#31#19#64#0#255#21#81#16#64#0#106#0#255#21#53#16#64#0);
 OutputCodeString(#214#18#64#0#25#17#64#0#70#17#64#0#196#17#64#0#39#18#64#0#57#18#64#0#157#18#64#0#191#18#64#0#199#18#64#0#144#141#64#0#144#141#64#0#144#141#64#0#144#141#64#0#106#246#255#21#57#16#64#0#163#39#19#64#0#106#5#80#255#21#61#16#64#0#106#245#255#21#57#16#64#0#163#43#19#64#0#106#3#80#255#21#61#16#64#0#137#37#210#18#64#0#255#21#73#16#64#0#163#31#19#64#0#104#28#0#64#0#104#12#0#1#0#80#255#21#77#16#64#0#163#35#19#64#0#141#160#0#0#64#0#139#236#190#251#18#64#0#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90#$90);
 OutputCodeDataSize:=1423;
end;

const locNone=0;
      locPushEAX=1;
      locPopEAX=2;
      locPopEBX=3;
      locIMulEBX=4;
      locXorEDXEDX=5;
      locIDivEBX=6;
      locPushEDX=7;
      locCmpEAXEBX=8;
      locMovzxEAXAL=9;
      locMovDWordPtrESPEAX=10;
      locJNZJNE0x03=11;
      locMovDWordPtrEBXEAX=12;
      locJmpDWordPtrESIOfs=13;
      locCallDWordPtrESIOfs=14;
      locXChgEDXESI=15;
      locPopESI=16;
      locMovECXImm=17;
      locCLD=18;
      locREPMOVSB=19;
      locTestEAXEAX=20;
      locNegDWordPtrESP=21;
      locMovEAXDWordPtrESP=22;
      locMovEBXDWordPtrFORStateCurrentValue=23;
      locCmpDWordPtrEBXEAX=24;
      locMovEAXDWordPtrFORStateDestValue=25;

var LastOutputCodeValue,PC:integer;

procedure OCPushEAX;
begin
 if LastOutputCodeValue=locPopEAX then begin
  if Code[PC]=OutputCodeDataSize then begin
   Code[PC]:=Code[PC]-1;
  end;
  if OutputCodeDataSize>0 then begin
   OutputCodeDataSize:=OutputCodeDataSize-1;
  end;
  LastOutputCodeValue:=locNone;
 end else begin
  EmitByte($50);
  LastOutputCodeValue:=locPushEAX;
 end;
end;

procedure OCPopEAX;
begin
 if LastOutputCodeValue=locPushEAX then begin
  if Code[PC]=OutputCodeDataSize then begin
   Code[PC]:=Code[PC]-1;
  end;
  if OutputCodeDataSize>0 then begin
   OutputCodeDataSize:=OutputCodeDataSize-1;
  end;
  LastOutputCodeValue:=locNone;
 end else begin
  EmitByte($58);
  LastOutputCodeValue:=locPopEAX;
 end;
end;

procedure OCPopEBX;
begin
 EmitByte($5b);
 LastOutputCodeValue:=locPopEBX;
end;

procedure OCIMulEBX;
begin
 EmitByte($f7);
 EmitByte($eb);
 LastOutputCodeValue:=locIMulEBX;
end;

procedure OCXorEDXEDX;
begin
 EmitByte($33); EmitByte($d2);
 LastOutputCodeValue:=locXorEDXEDX;
end;

procedure OCIDIVEBX;
begin
 EmitByte($f7); EmitByte($fb);
 LastOutputCodeValue:=locIDivEBX;
end;

procedure OCPushEDX;
begin
 EmitByte($52);
 LastOutputCodeValue:=locPushEDX;
end;

procedure OCCmpEAXEBX;
begin
 EmitByte($3b); EmitByte($c3);
 LastOutputCodeValue:=locCmpEAXEBX;
end;

procedure OCMovzxEAXAL;
begin
 EmitByte($0f); EmitByte($b6); EmitByte($c0);
 LastOutputCodeValue:=locMovzxEAXAL;
end;

procedure OCJNZJNE0x03;
begin
 EmitByte($75); EmitByte($03);
 LastOutputCodeValue:=locJNZJNE0x03;
end;

procedure OCMovDWordPtrESPEAX;
begin
 EmitByte($89); EmitByte($04); EmitByte($24);
 LastOutputCodeValue:=locMovDWordPtrESPEAX;
end;

procedure OCMovDWordPtrEBXEAX;
begin
 EmitByte($89); EmitByte($03);
 LastOutputCodeValue:=locMovDWordPtrEBXEAX;
end;

procedure OCJmpDWordPtrESIOfs(Ofs:integer);
begin
 EmitByte($ff); EmitByte($66); EmitByte(Ofs);
 LastOutputCodeValue:=locJmpDWordPtrESIOfs;
end;

procedure OCCallDWordPtrESIOfs(Ofs:integer);
begin
 EmitByte($ff); EmitByte($56); EmitByte(Ofs);
 LastOutputCodeValue:=locCallDWordPtrESIOfs;
end;

procedure OCXChgEDXESI;
begin
 EmitByte($87); EmitByte($d6);
 LastOutputCodeValue:=locXChgEDXESI;
end;

procedure OCPopESI;
begin
 EmitByte($5e);
 LastOutputCodeValue:=locPopESI;
end;

procedure OCMovECXImm(Value:integer);
begin
 EmitByte($b9); EmitInt32(Value);
 LastOutputCodeValue:=locMovECXImm;
end;

procedure OCCLD;
begin
 EmitByte($fc);
 LastOutputCodeValue:=locCLD;
end;

procedure OCREPMOVSB;
begin
 EmitByte($f3); EmitByte($a4);
 LastOutputCodeValue:=locREPMOVSB;
end;

procedure OCTestEAXEAX;
begin
 EmitByte($85); EmitByte($c0); { TEST EAX,EAX }
 LastOutputCodeValue:=locTestEAXEAX;
end;

procedure OCNegDWordPtrESP;
begin
 EmitByte($f7); EmitByte($1c); EmitByte($24); { NEG DWORD PTR [ESP] }
 LastOutputCodeValue:=locNegDWordPtrESP;
end;

procedure OCMovEAXDWordPtrESP;
begin
 EmitByte($8b); EmitByte($04); EmitByte($24); { MOV EAX,DWORD PTR [ESP] }
 LastOutputCodeValue:=locMovEAXDWordPtrESP;
end;

procedure OCMovEBXDWordPtrFORStateCurrentValue;
begin
 EmitByte($8b); EmitByte($5d); EmitByte($04);
 LastOutputCodeValue:=locMovEBXDWordPtrFORStateCurrentValue;
end;

procedure OCCmpDWordPtrEBXEAX;
begin
 EmitByte($39); EmitByte($03);
 LastOutputCodeValue:=locCmpDWordPtrEBXEAX;
end;

procedure OCMovEAXDWordPtrFORStateDestValue;
begin
 EmitByte($8b); EmitByte($45); EmitByte($08);
 LastOutputCodeValue:=locMovEAXDWordPtrFORStateDestValue;
end;

var JumpTable:array[1..MaximalCodeSize] of integer;

procedure AssembleAndLink;
var CountJumps,Opcode,Value,Index,PEEXECodeSize,PEEXESectionVirtualSize,PEEXESectionAlignment,PEEXECodeStart:integer;
begin
 EmitStubCode;
 PEEXECodeStart:=OutputCodeDataSize;
 LastOutputCodeValue:=locNone;
 PC:=0;
 CountJumps:=0;
 while PC<CodePosition do begin
  Opcode:=Code[PC];
  Value:=Code[PC+1];
  Code[PC]:=OutputCodeDataSize;
  case Opcode of
   OPAdd:begin
    OCPopEAX;
    EmitByte($01); EmitByte($04); EmitByte($24); { ADD DWORD PTR [ESP],EAX }
    LastOutputCodeValue:=locNone;
   end;
   OPNeg:begin
    OCNegDWordPtrESP;
   end;
   OPMul:begin
    OCPopEBX;
    OCPopEAX;
    OCIMulEBX;
    OCPushEAX;
   end;
   OPDivD:begin
    OCPopEBX;
    OCPopEAX;
    OCXorEDXEDX;
    OCIDIVEBX;
    OCPushEAX;
   end;
   OPRemD:begin
    OCPopEBX;
    OCPopEAX;
    OCXorEDXEDX;
    OCIDIVEBX;
    OCPushEDX;
   end;
   OPDiv2:begin
    EmitByte($d1); EmitByte($3c); EmitByte($24); { SAR DWORD PTR [ESP],1 }
    LastOutputCodeValue:=locNone;
   end;
   OPRem2:begin
    OCPopEAX;  
    EmitByte($8b); EmitByte($d8); { MOV EBX,EAX }
    EmitByte($25); EmitByte($01); EmitByte($00); EmitByte($00); EmitByte($80); { AND EAX,$80000001 }
    EmitByte($79); EmitByte($05); { JNS +$05 }
    EmitByte($48); { DEC EAX }
    EmitByte($83); EmitByte($c8); EmitByte($fe); { OR EAX,BYTE -$02 }
    EmitByte($40); { INC EAX }
    LastOutputCodeValue:=locNone;
    OCIMulEBX;
    OCPushEAX;
   end;
   OPEqlI:begin
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    EmitByte($0f); EmitByte($94); EmitByte($d0); { SETE AL }
    LastOutputCodeValue:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   end;
   OPNEqI:begin
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    EmitByte($0f); EmitByte($95); EmitByte($d0); { SETNE AL }
    LastOutputCodeValue:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   end;
   OPLssI:begin
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    EmitByte($0f); EmitByte($9c); EmitByte($d0); { SETL AL }
    LastOutputCodeValue:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   end;
   OPLeqI:begin
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    EmitByte($0f); EmitByte($9e); EmitByte($d0); { SETLE AL }
    LastOutputCodeValue:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   end;
   OPGtrI:begin
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    EmitByte($0f); EmitByte($9f); EmitByte($d0); { SETG AL }
    LastOutputCodeValue:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   end;
   OPGEqi:begin
    OCPopEBX;
    OCPopEAX;
    OCCmpEAXEBX;
    EmitByte($0f); EmitByte($9d); EmitByte($d0); { SETGE AL }
    LastOutputCodeValue:=locNone;
    OCMovzxEAXAL;
    OCPushEAX;
   end;
   OPDupl:begin
    EmitByte($ff); EmitByte($34); EmitByte($24); { PUSH DWORD PTR [ESP] }
    LastOutputCodeValue:=locNone;
   end;
   OPSwap:begin
    OCPopEBX;
    OCPopEAX;
    EmitByte($53); { PUSH EBX }
    LastOutputCodeValue:=locNone;
    OCPushEAX;
   end;
   OPAndB:begin
    OCPopEAX;
    OCTestEAXEAX;
    OCJNZJNE0x03;
    OCMovDWordPtrESPEAX;
    LastOutputCodeValue:=locNone;
   end;
   OPOrB:begin
    OCPopEAX;
    EmitByte($83); EmitByte($f8); EmitByte($01);  { CMP EAX,1 }
    LastOutputCodeValue:=locNone;
    OCJNZJNE0x03;
    OCMovDWordPtrESPEAX;
    LastOutputCodeValue:=locNone;
   end;
   OPLoad:begin
    OCPopEAX;
    EmitByte($ff); EmitByte($30); { PUSH DWORD PTR [EAX] }
    LastOutputCodeValue:=locNone;
   end;
   OPStore:begin
    OCPopEBX;
    OCPopEAX;
    OCMovDWordPtrEBXEAX;
   end;
   OPHalt:begin
    OCJmpDWordPtrESIOfs(0);
   end;
   OPWrI:begin
    OCCallDWordPtrESIOfs(8);
   end;
   OPWrC:begin
    OCCallDWordPtrESIOfs(4);
   end;
   OPWrL:begin
    OCCallDWordPtrESIOfs(12);
   end;
   OPRdI:begin
    OCPopEBX;
    OCCallDWordPtrESIOfs(20);
    OCMovDWordPtrEBXEAX;
   end;
   OPRdC:begin
    OCPopEBX;
    OCCallDWordPtrESIOfs(16);
    OCMovzxEAXAL;
    OCMovDWordPtrEBXEAX;
   end;
   OPRdL:begin
    OCCallDWordPtrESIOfs(24);
   end;
   OPEOF:begin
    OCCallDWordPtrESIOfs(28);
    OCPushEAX;
   end;
   OPEOL:begin
    OCCallDWordPtrESIOfs(32);
    OCPushEAX;
   end;
   OPLdC:begin
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($6a); EmitByte(Value); { PUSH BYTE Value }
    end else begin
     EmitByte($68); EmitInt32(Value); { PUSH DWORD Value }
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPLdA:begin
    if Value=0 then begin
     EmitByte($8b); EmitByte($c5); { MOV EAX,EBP }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($8d); EmitByte($45); EmitByte(Value); { LEA EAX,[EBP+BYTE Value] }
    end else begin
     EmitByte($8d); EmitByte($85); EmitInt32(Value); { LEA EAX,[EBP+DWORD Value] }
    end;
    LastOutputCodeValue:=locNone;
    OCPushEAX;
    PC:=PC+1;
   end;
   OPLdLA:begin
    if Value=0 then begin
     EmitByte($8b); EmitByte($c4); { MOV EAX,ESP }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($8d); EmitByte($44); EmitByte($24); EmitByte(Value); { LEA EAX,[ESP+BYTE Value] }
    end else begin
     EmitByte($8d); EmitByte($84); EmitByte($24); EmitInt32(Value); { LEA EAX,[ESP+DWORD Value] }
    end;
    LastOutputCodeValue:=locNone;
    OCPushEAX;
    PC:=PC+1;
   end;
   OPLdL:begin
    if Value=0 then begin
     OCMovEAXDWordPtrESP;
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($8b); EmitByte($44); EmitByte($24); EmitByte(Value); { MOV EAX,DWORD PTR [ESP+BYTE Value] }
    end else begin
     EmitByte($8b); EmitByte($84); EmitByte($24); EmitInt32(Value); { MOV EAX,DWORD PTR [ESP+DWORD Value] }
    end;
    OCPushEAX;
    PC:=PC+1;
   end;
   OPLdG:begin
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($8b); EmitByte($45); EmitByte(Value); { MOV EAX,DWORD PTR [EBP+BYTE Value] }
    end else begin
     EmitByte($8b); EmitByte($85); EmitInt32(Value); { MOV EAX,DWORD PTR [EBP+DWORD Value] }
    end;
    LastOutputCodeValue:=locNone;
    OCPushEAX;
    PC:=PC+1;
   end;
   OPStL:begin
    OCPopEAX;
    Value:=Value-4;
    if Value=0 then begin
     EmitByte($89); EmitByte($04); EmitByte($24); { MOV DWORD PTR [ESP],EAX }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($89); EmitByte($44); EmitByte($24); EmitByte(Value); { MOV DWORD PTR [ESP+BYTE Value],EAX }
    end else begin
     EmitByte($89); EmitByte($84); EmitByte($24); EmitInt32(Value); { MOV EAX,DWORD PTR [ESP+DWORD Value] }
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPStG:begin
    OCPopEAX;
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($89); EmitByte($45); EmitByte(Value); { MOV DWORD PTR [EBP+BYTE Value],EAX }
    end else begin
     EmitByte($89); EmitByte($85); EmitInt32(Value); { MOV EAX,DWORD PTR [EBP+DWORD Value] }
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPMove:begin
    OCXChgEDXESI;
    EmitByte($5f); { POP EDI }
    LastOutputCodeValue:=locNone;
    OCPopESI;
    OCMovECXImm(Value);
    OCCLD;
    OCREPMOVSB;
    OCXChgEDXESI;
    PC:=PC+1;
   end;
   OPCopy:begin
    OCXChgEDXESI;
    OCPopESI;
    OCMovECXImm(Value);
    EmitByte($2b); EmitByte($e1); { SUB ESP,ECX }
    EmitByte($8b); EmitByte($fc); { MOV EDI,ESP }
    LastOutputCodeValue:=locNone;
    OCCLD;
    OCREPMOVSB;
    OCXChgEDXESI;
    PC:=PC+1;
   end;
   OPAddC:begin
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($83); EmitByte($04); EmitByte($24); EmitByte(Value); { ADD DWORD PTR [ESP],BYTE Value }
    end else begin
     EmitByte($81); EmitByte($04); EmitByte($24); EmitInt32(Value); { ADD DWORD PTR [ESP],DWORD Value }
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPMulC:begin
    if Value=(-1) then begin
     OCNegDWordPtrESP;
    end else if (Value>=-128) and (Value<=127) then begin
     OCPopEAX;
     EmitByte($6b); EmitByte($c0); EmitByte(Value); { IMUL EAX,BYTE s }
     LastOutputCodeValue:=locNone;
     OCPushEAX;
    end else begin
     OCPopEAX;
     EmitByte($69); EmitByte($c0); EmitInt32(Value); { IMUL EAX,DWORD s }
     LastOutputCodeValue:=locNone;
     OCPushEAX;
    end;
    PC:=PC+1;
   end;
   OPJmp:begin
    if Value<>(PC+2) then begin
     CountJumps:=CountJumps+1;
     EmitByte($e9); { JMP Value }
     JumpTable[CountJumps]:=OutputCodeDataSize+1;
     EmitInt32(Value);
    end;
    PC:=PC+1;
    LastOutputCodeValue:=locNone;
   end;
   OPJZ:begin
    CountJumps:=CountJumps+1;
    OCPopEAX;
    OCTestEAXEAX;
    EmitByte($0f); EmitByte($84); { JZ Value }
    JumpTable[CountJumps]:=OutputCodeDataSize+1;
    EmitInt32(Value);
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPCall:begin
    CountJumps:=CountJumps+1;
    EmitByte($e8); { CALL Value }
    JumpTable[CountJumps]:=OutputCodeDataSize+1;
    EmitInt32(Value);
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPAdjS:begin
    if Value>0 then begin
     if (Value>=-128) and (Value<=127) then begin
      EmitByte($83); EmitByte($c4); EmitByte(Value); { ADD ESP,BYTE Value }
     end else begin
      EmitByte($81); EmitByte($c4); EmitInt32(Value); { ADD ESP,DWORD Value }
     end;
    end else if Value<0 then begin
     Value:=-Value;
     if (Value>=-128) and (Value<=127) then begin
      EmitByte($83); EmitByte($ec); EmitByte(Value); { SUB ESP,BYTE Value }
     end else begin
      EmitByte($81); EmitByte($ec); EmitInt32(Value); { SUB ESP,DWORD Value }
     end;
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
   OPExit:begin
    Value:=Value-4;
    if Value>0 then begin
     EmitByte($c2); EmitInt16(Value); { RET Value }
    end else if Value=0 then begin
     EmitByte($c3); { RET }
    end else begin
     Error(145);
    end;
    LastOutputCodeValue:=locNone;
    PC:=PC+1;
   end;
  end;
  PC:=PC+1;
 end;

 { Patch jumps + calls }
 for Index:=1 to CountJumps do begin
  Value:=JumpTable[Index];
  OutputCodePutInt32(Value,((Code[OutputCodeGetInt32(Value)]-Value)-3));
 end;

 { Size Of Code }
 PEEXECodeSize:=OutputCodeGetInt32($29)+(OutputCodeDataSize-PEEXECodeStart);
 OutputCodePutInt32($29,PEEXECodeSize);

 { Get section alignment }
 PEEXESectionAlignment:=OutputCodeGetInt32($45);

 { Calculate and patch section virtual size }
 PEEXESectionVirtualSize:=PEEXECodeSize;
 if PEEXESectionAlignment<>0 then begin
  Value:=PEEXECodeSize mod PEEXESectionAlignment;
  if Value<>0 then begin
   PEEXESectionVirtualSize:=PEEXESectionVirtualSize+(PEEXESectionAlignment-Value);
  end;
 end;
 OutputCodePutInt32($10d,PEEXESectionVirtualSize);

 { Calculate and patch image size }
 OutputCodePutInt32($5d,PEEXESectionVirtualSize+OutputCodeGetInt32($39));

 { Patch section raw size }
 OutputCodePutInt32($115,OutputCodeGetInt32($115)+(OutputCodeDataSize-PEEXECodeStart));

 WriteOutputCode;
end;

begin
 StringCopy(Keywords[SymBEGIN],'BEGIN               ');
 StringCopy(Keywords[SymEND],'END                 ');
 StringCopy(Keywords[SymIF],'IF                  ');
 StringCopy(Keywords[SymTHEN],'THEN                ');
 StringCopy(Keywords[SymELSE],'ELSE                ');
 StringCopy(Keywords[SymWHILE],'WHILE               ');
 StringCopy(Keywords[SymDO],'DO                  ');
 StringCopy(Keywords[SymCASE],'CASE                ');
 StringCopy(Keywords[SymREPEAT],'REPEAT              ');
 StringCopy(Keywords[SymUNTIL],'UNTIL               ');
 StringCopy(Keywords[SymFOR],'FOR                 ');
 StringCopy(Keywords[SymTO],'TO                  ');
 StringCopy(Keywords[SymDOWNTO],'DOWNTO              ');
 StringCopy(Keywords[SymNOT],'NOT                 ');
 StringCopy(Keywords[SymDIV],'DIV                 ');
 StringCopy(Keywords[SymMOD],'MOD                 ');
 StringCopy(Keywords[SymAND],'AND                 ');
 StringCopy(Keywords[SymOR],'OR                  ');
 StringCopy(Keywords[SymCONST],'CONST               ');
 StringCopy(Keywords[SymVAR],'VAR                 ');
 StringCopy(Keywords[SymTYPE],'TYPE                ');
 StringCopy(Keywords[SymARRAY],'ARRAY               ');
 StringCopy(Keywords[SymOF],'OF                  ');
 StringCopy(Keywords[SymPACKED],'PACKED              ');
 StringCopy(Keywords[SymRECORD],'RECORD              ');
 StringCopy(Keywords[SymPROGRAM],'PROGRAM             ');
 StringCopy(Keywords[SymFORWARD],'FORWARD             ');
 StringCopy(Keywords[SymHALT],'HALT                ');
 StringCopy(Keywords[SymFUNC],'FUNCTION            ');
 StringCopy(Keywords[SymPROC],'PROCEDURE           ');

 Types[TypeINT].Size:=4;
 Types[TypeINT].Kind:=KindSIMPLE;
 Types[TypeCHAR].Size:=4;
 Types[TypeCHAR].Kind:=KindSIMPLE;
 Types[TypeBOOL].Size:=4;
 Types[TypeBOOL].Kind:=KindSIMPLE;
 Types[TypeSTR].Size:=0;
 Types[TypeSTR].Kind:=KindSIMPLE;
 TypePosition:=4;

 SymbolNameList[-1]:=0;
 CurrentLevel:=-1;
 IdentifierPosition:=0;

 EnterSymbol('FALSE               ',IdCONST,TypeBOOL);
 Identifiers[IdentifierPosition].Value:=ord(false);

 EnterSymbol('TRUE                ',IdCONST,TypeBOOL);
 Identifiers[IdentifierPosition].Value:=ord(true);

 EnterSymbol('MAXINT              ',IdCONST,TypeINT);
 Identifiers[IdentifierPosition].Value:=2147483647;

 EnterSymbol('INTEGER             ',IdTYPE,TypeINT);
 EnterSymbol('CHAR                ',IdTYPE,TypeCHAR);
 EnterSymbol('BOOLEAN             ',IdTYPE,TypeBOOL);

 EnterSymbol('CHR                 ',IdFUNC,TypeCHAR);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunCHR;
 Identifiers[IdentifierPosition].Inside:=false;

 EnterSymbol('ORD                 ',IdFUNC,TypeINT);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunORD;
 Identifiers[IdentifierPosition].Inside:=false;

 EnterSymbol('WRITE               ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunWRITE;

 EnterSymbol('WRITELN             ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunWRITELN;

 EnterSymbol('READ                ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunREAD;

 EnterSymbol('READLN              ',IdFUNC,0);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunREADLN;

 EnterSymbol('EOF                 ',IdFUNC,TypeBOOL);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunEOF;
 Identifiers[IdentifierPosition].Inside:=false;

 EnterSymbol('EOLN                ',IdFUNC,TypeBOOL);
 Identifiers[IdentifierPosition].FunctionLevel:=-1;
 Identifiers[IdentifierPosition].FunctionAddress:=FunEOFLN;
 Identifiers[IdentifierPosition].Inside:=false;

 SymbolNameList[0]:=0;
 CurrentLevel:=0;

 CurrentLine:=1;
 CurrentColumn:=0;

 ReadChar;
 GetSymbol;
 IsLabeled:=true;
 CodePosition:=0;
 LastOpcode:=-1;
 StackPosition:=4;
 Expect(SymPROGRAM);
 Expect(TokIdent);
 Expect(TokSemi);
 EmitOpcode(OPJmp,0);
 Block(0);
 EmitOpcode2(OPHalt);
 Check(TokPeriod);
 AssembleAndLink;
end.
