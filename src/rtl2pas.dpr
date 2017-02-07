program rtl2pas;

const LineLen=255;
      SigStart=4;
      SigLen=7;

type TSignature = array[1..SigLen] of char;

procedure MakeRtl(sig: TSignature);
var CurChar: char;
    j, datasize: integer;
begin
  writeln(' OutputCodeDataSize:=0;');
  j:=0;
  while not EOF do begin
    if (j mod LineLen) = 0 then begin
      write(' OutputCodeString(');
    end;
    read(CurChar);
    if (SigStart <= j) and (j < SigStart + SigLen) then begin
      write('#', ord(sig[j - SigStart + 1]));
    end else begin
      write('#', ord(CurChar));
    end;
    j:=j+1;
    if (j mod LineLen) = 0 then begin
      writeln(');');
    end;
  end;

  datasize:=j;
  while (j mod LineLen) <> 0 do begin
    write('#$90');
    j:=j+1;
  end;
  writeln(');');
  writeln(' OutputCodeDataSize:=', datasize, ';');
end;

begin
  MakeRtl('BeRo^fr');
end.
