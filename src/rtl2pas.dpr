program rtl2pas;
uses SysUtils,Classes;
const Signature:array[0..7] of ansichar='BeRo^fr';
var Stream:TMemoryStream;
    StringList:TStringList;
    i,j,k:longint;
    l:ansistring;
begin
 Stream:=TMemoryStream.Create;
 try
  Stream.LoadFromFile('rtl.exe');
  Move(Signature[0],PAnsiChar(Stream.Memory)[4],SizeOf(Signature));
  StringList:=TStringList.Create;
  try
   StringList.Add(' OutputCodeDataSize:=0;');
   l:='';
   j:=0;
   k:=Stream.Size;
   if (k mod 255)<>0 then begin
    inc(k,255-(k mod 255));
   end;
   for i:=0 to k-1 do begin
    if length(l)=0 then begin
     l:=' OutputCodeString(';
    end;
    if i<Stream.Size then begin
     l:=l+'#'+IntToStr(Byte(AnsiChar(PAnsiChar(Stream.Memory)[i])));
    end else begin
     l:=l+'#$90';
    end;
    inc(j);
    if j>=255 then begin
     j:=0;
     if length(l)>0 then begin
      StringList.Add(l+');');
     end;
     l:='';
    end;
   end;
   if length(l)>0 then begin
    StringList.Add(l+');');
   end;
   StringList.Add(' OutputCodeDataSize:='+IntToStr(Stream.Size)+';');
   StringList.SaveToFile('rtl.pas');
  finally
   StringList.Free;
  end;
 finally
  Stream.Free;
 end;
end.
