program Example;
{$MODE OBJFPC}{$H+}
uses
  FPLIL;
var
  LIL: TLIL;

function FncMul(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 2 then exit(nil);
  Result:=TLIL.AllocFloat(TLIL.ToFloat(Args[0])*TLIL.ToFloat(Args[1]));
end;

begin
  LIL:=TLIL.Create(nil);
  LIL.Register('mul', @FncMul);
  LIL.Parse('print [mul 3 4]').Free;
  LIL.Free;
end.

