unit lazliltestmainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, FPLIL;

type

  { TMain }

  TMain = class(TForm)
    Button1: TButton;
    LIL1: TLIL;
    mOutput: TMemo;
    mCode: TMemo;
    Panel1: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure LIL1Error(LIL: TLIL; APosition: Integer; AMessage: string);
    procedure LIL1Exit(LIL: TLIL; Arg: TLILValue);
    procedure LIL1Write(LIL: TLIL; Chars: string);
  private
    { private declarations }
  public
    { public declarations }
    LineBuffer: string;
  end; 

var
  Main: TMain;

implementation

{$R *.lfm}

{ TMain }

procedure TMain.LIL1Exit(LIL: TLIL; Arg: TLILValue);
begin
  Close;
end;

procedure TMain.LIL1Write(LIL: TLIL; Chars: string);
var
  i: Integer;
begin
  for i:=1 to Length(Chars) do
    if Chars[i]=#10 then begin
      mOutput.Lines.Add(LineBuffer);
      LineBuffer:=''
    end else begin
      LineBuffer:=LineBuffer + Chars[i];
    end;
end;

procedure TMain.LIL1Error(LIL: TLIL; APosition: Integer; AMessage: string);
begin
  ShowMessage('Error in evaluated code at ' + IntToStr(APosition) + ': ' + AMessage);
end;

procedure TMain.Button1Click(Sender: TObject);
begin
  LineBuffer:='';
  LIL1.Parse(mCode.Text, True);
  if LineBuffer <> '' then mOutput.Lines.Add(LineBuffer);
end;

end.

