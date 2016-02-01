program lil;
{$MODE OBJFPC}{$H+}
uses SysUtils, fplil;
var
  Running: Boolean = True;

function FncWriteChar(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args)=0 then exit(nil);
  Write(Chr(Args[0].IntegerValue));
  Result:=nil;
end;

function FncReadLine(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Line: string;
begin
  Readln(Line);
  Result:=TLIL.AllocString(Line);
end;

procedure REPL;
var
  Command: string;
  LIL: TLIL;
  RetVal: TLILValue;
begin
  LIL:=TLIL.Create(nil);
  LIL.Register('writechar', @FncWriteChar);
  LIL.Register('readline', @FncReadLine);
  while Running do begin
    Write('# ');
    ReadLn(Command);
    if Command='' then continue;
    RetVal:=LIL.Parse(Command);
    if LIL.Error then begin
      WriteLn('Error: ' + LIL.ErrorMessage);
    end;
    if RetVal <> nil then begin
      if (not LIL.Error) and (RetVal.Length > 0) then WriteLn(RetVal.StringValue);
      RetVal.Free;
    end;
  end;
  LIL.Free;
end;

procedure NonInteractive;
var
  LIL: TLIL;
  FileName: string;
  ArgList: TLILList;
  Args, Result: TLILValue;
  TmpCode: string;
  i: Integer;
begin
  LIL:=TLIL.Create(nil);
  LIL.Register('writechar', @FncWriteChar);
  FileName:=ParamStr(1);
  ArgList:=TLILList.Create;
  for i:=2 to ParamCount do ArgList.AddString(ParamStr(i));
  Args:=ArgList.ToValue;
  FreeAndNil(ArgList);
  LIL.SetVar('argv', Args, lsvlGlobal);
  FreeAndNil(Args);
  TmpCode:='set __lilmain:code__ [read {' + FileName + '}]'#10'if [streq $__lilmain:code__ ''] {print There is no code in the file or the file does not exist} {eval $__lilmain:code__}'#10;
  Result:=LIL.Parse(TmpCode);
  FreeAndNil(Result);
  if LIL.Error then WriteLn('lil: error at ', LIL.ErrorHead, ': ', LIL.ErrorMessage);
  FreeAndNil(LIL);
end;

begin
  if ParamCount=0 then begin
    WriteLn('FreePascal implementation of LIL');
    WriteLn('Type "exit" to exit');
    WriteLn;
    REPL;
  end else NonInteractive;
end.
