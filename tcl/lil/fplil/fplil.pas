unit FPLIL;
{$MODE OBJFPC}{$H+}
interface
uses Classes, SysUtils;
const
  LIL_VERSION_STRING = '0.1';
type
  TLILValue = class;
  TLILVariable = class;
  TLILEnvironment = class;
  TLILFunction = class;
  TLIL = class;

  TLILFunctionProcArgs = array of TLILValue;
  TLILFunctionProc = function(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;

  TLILOnSetVarAction = (losaDefault, losaReplace, losaIgnore);
  TLILOnGetVarAction = (logaDefault, logaReplace);

  TLILOnExit = procedure(LIL: TLIL; Arg: TLILValue) of object;
  TLILOnWrite = procedure(LIL: TLIL; Chars: string) of object;
  TLILOnRead = procedure(LIL: TLIL; AFileName: string; out AContent: string) of object;
  TLILOnStore = procedure(LIL: TLIL; AFileName, AContent: string) of object;
  TLILOnSource = procedure(LIL: TLIL; AFileName: string; out AContent: string) of object;
  TLILOnError = procedure(LIL: TLIL; APosition: Integer; AMessage: string) of object;
  TLILOnSetVar = procedure(LIL: TLIL; AName: string; const AValue: TLILValue; var AReplacement: TLILValue; var ASetVarAction: TLILOnSetVarAction) of object;
  TLILOnGetVar = procedure(LIL: TLIL; AName: string; const AValue: TLILValue; var AReplacement: TLILValue; var AGetVarAction: TLILOnGetVarAction) of object;

  TLILSetVarLocality = (lsvlGlobal, lsvlLocal, lsvlLocalNew, lsvlLocalOnly);

  TLILValue = class
  private
    FData: string;
    FLength: Integer;
    function ToInteger: Int64;
    function ToFloat: Extended;
    function ToBoolean: Boolean;
  public
    procedure AppendChar(Ch: Char);
    procedure AppendString(Str: string);
    procedure AppendValue(AValue: TLILValue);
    function Clone: TLILValue;
    function Equals(StrVal: string): Boolean;
    function Equals(IntVal: Int64): Boolean;
    function Equals(FloatVal: Extended): Boolean;
    function Equals(LILVal: TLILValue): Boolean;
    property StringValue: string read FData;
    property IntegerValue: Int64 read ToInteger;
    property FloatValue: Extended read ToFloat;
    property BooleanValue: Boolean read ToBoolean;
    property Length: Integer read FLength;
  end;

  TLILVariable = class
  private
    FName: string;
    FEnvironment: TLILEnvironment;
    FValue: TLILValue;
    procedure SetValue(AValue: TLILValue);
  public
    constructor Create(AName: string; AEnvironment: TLILEnvironment);
    destructor Destroy; override;
    property Name: string read FName;
    property Environment: TLILEnvironment read FEnvironment;
    property Value: TLILValue read FValue write SetValue;
  end;

  TLILEnvironment = class
  private
    Parent: TLILEnvironment;
    Func: TLILFunction;
    CatcherFor: TLILValue;
    Vars: TFPList;
    RetVal: TLILValue;
    RetValSet: Boolean;
    BreakRun: Boolean;
    procedure RegisterVariable(AVariable: TLILVariable);
  public
    constructor Create(AParent: TLILEnvironment);
    destructor Destroy; override;
  end;

  { TLILList }

  TLILList = class
  private
    FValues: TFPList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): TLILValue;
    function ToFunctionArgs: TLILFunctionProcArgs;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AValue: TLILValue);
    procedure AddString(AString: string);
    procedure AddInteger(AInteger: Int64);
    procedure AddFloat(AFloat: Extended);
    procedure AddBoolean(ABoolean: Boolean);
    function IndexOf(AValue: TLILValue): Integer;
    function IndexOfString(AString: string): Integer;
    function IndexOfInteger(AInteger: Int64): Integer;
    function IndexOfFloat(AFloat: Extended): Integer;
    function IndexOfBoolean(ABoolean: Boolean): Integer;
    function Has(AValue: TLILValue): Boolean; inline;
    function HasString(AString: string): Boolean; inline;
    function HasInteger(AInteger: Int64): Boolean; inline;
    function HasFloat(AFloat: Extended): Boolean; inline;
    function HasBoolean(ABoolean: Boolean): Boolean; inline;
    function ToValue(DoEscape: Boolean=True): TLILValue;
    property Count: Integer read GetCount;
    property Values[AIndex: Integer]: TLILValue read GetValue; default;
  end;

  TLILFunction = class
  private
    FName: string;
    Code: TLILValue;
    ArgNames: TLILList;
    Proc: TLILFunctionProc;
    function GetBody: string; inline;
    function IsNative: Boolean; inline;
    function GetArguments: Integer; inline;
    function GetArgument(AIndex: Integer): string; inline;
  public
    constructor Create;
    destructor Destroy; override;
    property Name: string read FName;
    property Body: string read GetBody;
    property Native: Boolean read IsNative;
    property FunctionProc: TLILFunctionProc read Proc;
    property Arguments: Integer read GetArguments;
    property Argument[AIndex: Integer]: string read GetArgument;
  end;

  TLIL = class(TComponent)
  private
    Code: string;
    RootCode: string;
    CLen: Integer;
    Head: Integer;
    Funcs: array of TLILFunction;
    FFuncName: string;
    SysFuncs: Integer;
    Catcher: string;
    InCatcher: Integer;
    DollarPrefix: string;
    Env: TLILEnvironment;
    RootEnv: TLILEnvironment;
    DownEnv: TLILEnvironment;
    Empty: TLILValue;
    FError: Boolean;
    FErrorHead: Integer;
    FErrorMessage: string;
    ParseDepth: Integer;
    FData: Pointer;
    FixHead: Boolean;
    FOnExit: TLILOnExit;
    FOnWrite: TLILOnWrite;
    FOnRead: TLILOnRead;
    FOnStore: TLILOnStore;
    FOnSource: TLILOnSource;
    FOnError: TLILOnError;
    FOnSetVar: TLILOnSetVar;
    FOnGetVar: TLILOnGetVar;
    procedure RegisterStandardFunctions;
    function FindLocalVar(AEnv: TLILEnvironment; AName: string): TLILVariable;
    function FindVar(AEnv: TLILEnvironment; AName: string): TLILVariable;
    function FindFunction(AName: string): TLILFunction;
    function AddFunction(AName: string): TLILFunction;
    function AtEol: Boolean;
    procedure SkipSpaces;
    function GetBracketPart: TLILValue;
    function GetDollarPart: TLILValue;
    function NextWord: TLILValue;
    function Substitute: TLILList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function AllocString(Str: string): TLILValue; inline;
    class function AllocInteger(Int: Int64): TLILValue; inline;
    class function AllocFloat(Float: Extended): TLILValue; inline;
    class function AllocBoolean(Bool: Boolean): TLILValue; inline;
    class function ToString(AValue: TLILValue): string; inline;
    class function ToInteger(AValue: TLILValue): Int64; inline;
    class function ToFloat(AValue: TLILValue): Extended; inline;
    class function ToBoolean(AValue: TLILValue): Boolean; inline;
    class function Clone(AValue: TLILValue): TLILValue; inline;

    procedure SetError(AErrorMessage: string);

    procedure Register(AName: string; AProc: TLILFunctionProc);
    function GetFunction(AName: string): TLILFunction; inline;
    function SetVar(AName: string; AValue: TLILValue; Locality: TLILSetVarLocality): TLILVariable;
    function GetVar(AName: string; DefaultValue: TLILValue=nil): TLILValue;

    function UnusedName(Part: string): string;

    procedure PushEnv;
    procedure PopEnv;

    function SubstituteToList(ACode: TLILValue): TLILList;
    function SubstituteToValue(ACode: TLILValue): TLILValue;

    function EvaluateExpression(ACode: string): TLILValue;
    function EvaluateExpressionValue(AValue: TLILValue): TLILValue;

    function Parse(ACommand: string; FuncLevel: Boolean=False): TLILValue;
    function ParseValue(ACmdValue: TLILValue; FuncLevel: Boolean=False): TLILValue;

    function ToString: string; override;

    property FuncName: string read FFuncName;
    property Error: Boolean read FError;
    property ErrorHead: Integer read FErrorHead;
    property ErrorMessage: string read FErrorMessage;
    property Data: Pointer read FData write FData;

  published
    property EvaluatedCode: string read Code;
    property OnExit: TLILOnExit read FOnExit write FOnExit;
    property OnWrite: TLILOnWrite read FOnWrite write FOnWrite;
    property OnRead: TLILOnRead read FOnRead write FOnRead;
    property OnStore: TLILOnStore read FOnStore write FOnStore;
    property OnSource: TLILOnSource read FOnSource write FOnSource;
    property OnError: TLILOnError read FOnError write FOnError;
    property OnSetVar: TLILOnSetVar read FOnSetVar write FOnSetVar;
    property OnGetVar: TLILOnGetVar read FOnGetVar write FOnGetVar;
  end;

var
  LILFormatSettings: TFormatSettings;

procedure Register;

implementation
{$HINTS-}
{$COPERATORS ON}
{$IFDEF LAZFPLIL}
uses
  LResources;
  {$ENDIF}

const
  MAX_CATCHER_DEPTH = 16384;

function Sign(a: Extended): Extended; inline;
begin
  if a < 0 then Result:=-1 else Result:=1;
end;

function FMod(a, b: Extended): Extended; inline;
begin
  Result:=Sign(a)*(Abs(a) - Trunc(Abs(a/b))*Abs(b));
end;

function IsPunct(Ch: Char): Boolean; inline;
begin
  Result:=Ch in [#33, #34, #35, #36, #37, #38, #39, #40, #41, #42, #43, #44, #45, #46, #47, #58,
    #59, #60, #61, #62, #63, #64, #91, #92, #93, #94, #95, #96, #123, #124, #125, #126];
end;

function IsSpace(Ch: Char): Boolean; inline;
begin
  Result:=Ch in [#9..#13, #32];
end;

function IsDigit(Ch: Char): Boolean; inline;
begin
  Result:=Ch in ['0'..'9'];
end;

function IsXDigit(Ch: Char): Boolean; inline;
begin
  Result:=Ch in ['0'..'9', 'A'..'F', 'a'..'f'];
end;

function IsAlpha(Ch: Char): Boolean; inline;
begin
  Result:=Ch in ['A'..'Z', 'a'..'z'];
end;

function IsAlNum(Ch: Char): Boolean; inline;
begin
  Result:=IsAlpha(Ch) or IsDigit(Ch);
end;

function IsIdpart(Ch: Char): Boolean; inline;
begin
  Result:=IsAlNum(Ch) or (Ch='_');
end;

function IsSpecial(Ch: Char): Boolean; inline;
begin
  Result:=Ch in [';', '$', '[', ']', '{', '}', '"', ''''];
end;

{ Standard functions }
function FncReflect(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  RefType: string;
  Func: TLILFunction;
  Funcs, Vars: TLILList;
  Env: TLILEnvironment;
  i: Integer;
begin
  Result:=nil;
  if Length(Args)=0 then exit;
  RefType:=Args[0].StringValue;
  if RefType='version' then begin
    Result:=TLIL.AllocString(LIL_VERSION_STRING);
  end else if RefType='args' then begin
    if Length(Args) < 2 then exit;
    Func:=LIL.FindFunction(Args[1].StringValue);
    if not Assigned(Func) or not Assigned(Func.ArgNames) then exit;
    Result:=Func.ArgNames.ToValue;
  end else if RefType='body' then begin
    if Length(Args) < 2 then exit;
    Func:=LIL.FindFunction(Args[1].StringValue);
    if not Assigned(Func) or Func.Native then exit;
    Result:=TLIL.Clone(Func.Code);
  end else if RefType='func-count' then begin
    Result:=TLIL.AllocInteger(Length(LIL.Funcs));
  end else if RefType='funcs' then begin
    Funcs:=TLILList.Create;
    for i:=0 to Length(LIL.Funcs) - 1 do
      Funcs.AddString(LIL.Funcs[i].Name);
    Result:=Funcs.ToValue;
    FreeAndNil(Funcs);
  end else if RefType='vars' then begin
    Env:=LIL.Env;
    Vars:=TLILList.Create;
    for i:=0 to Env.Vars.Count - 1 do
      Vars.AddString(TLILVariable(Env.Vars[i]).Name);
    Result:=Vars.ToValue;
    FreeAndNil(Vars);
  end else if RefType='globals' then begin
    Env:=LIL.RootEnv;
    Vars:=TLILList.Create;
    for i:=0 to Env.Vars.Count - 1 do
      Vars.AddString(TLILVariable(Env.Vars[i]).Name);
    Result:=Vars.ToValue;
    FreeAndNil(Vars);
  end else if RefType='has-func' then begin
    if Length(Args) < 2 then exit;
    if Assigned(LIL.FindFunction(Args[1].StringValue)) then
      Result:=TLIL.AllocString('1');
  end else if RefType='has-var' then begin
    if Length(Args) < 2 then exit;
    if Assigned(LIL.FindVar(LIL.Env, Args[1].StringValue)) then
      Result:=TLIL.AllocString('1');
  end else if RefType='has-global' then begin
    if Length(Args) < 2 then exit;
    if Assigned(LIL.FindVar(LIL.RootEnv, Args[1].StringValue)) then
      Result:=TLIL.AllocString('1');
  end else if RefType='error' then begin
    if LIL.Error then
      Result:=TLIL.AllocString(LIL.ErrorMessage);
  end else if RefType='dollar-prefix' then begin
    Result:=TLIL.AllocString(LIL.DollarPrefix);
    if Length(Args) > 1 then LIL.DollarPrefix:=Args[1].StringValue;
  end else if RefType='this' then begin
    Env:=LIL.Env;
    while (Env <> LIL.RootEnv) and not Assigned(Env.CatcherFor) and not Assigned(Env.Func) do Env:=Env.Parent;
    if Assigned(Env.CatcherFor) then Result:=TLIL.AllocString(LIL.Catcher)
    else if Env=LIL.RootEnv then Result:=TLIL.AllocString(LIL.RootCode)
    else if Assigned(Env.Func) then Result:=Env.Func.Code;
  end else if RefType='name' then begin
    Env:=LIL.Env;
    while (Env <> LIL.RootEnv) and not Assigned(Env.CatcherFor) and not Assigned(Env.Func) do Env:=Env.Parent;
    if Assigned(Env.CatcherFor) then Result:=Env.CatcherFor
    else if Env=LIL.RootEnv then Result:=nil
    else if Assigned(Env.Func) then Result:=TLIL.AllocString(Env.Func.Name);
  end;
end;

function FncFunc(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Name: string;
  Func: TLILFunction;
begin
  if Length(Args)=0 then exit(nil);
  if Length(Args)=3 then begin
    Result:=TLIL.Clone(Args[0]);
    Func:=LIL.AddFunction(TLIL.ToString(Result));
    Func.ArgNames:=LIL.SubstituteToList(Args[1]);
    Func.Code:=TLIL.Clone(Args[2]);
  end else begin
    Name:=LIL.UnusedName('anonymous-function');
    if Name='' then exit(nil);
    Func:=LIL.AddFunction(Name);
    Result:=TLIL.AllocString(Name);
    if Length(Args) < 2 then begin
      Func.ArgNames:=TLILList.Create;
      Func.ArgNames.AddString('args');
      Func.Code:=TLIL.Clone(Args[0]);
    end else begin
      Func.ArgNames:=LIL.SubstituteToList(Args[0]);
      Func.Code:=TLIL.Clone(Args[1]);
    end;
  end;
end;

function FncRename(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Func: TLILFunction;
begin
  if Length(Args) < 2 then exit(nil);
  Func:=LIL.FindFunction(TLIL.ToString(Args[0]));
  if Func=nil then begin
    LIL.SetError('Unknown function ''' + TLIL.ToString(Args[0]) + '''');
    exit(nil);
  end;
  Result:=TLIL.AllocString(Func.Name);
  Func.FName:=TLIL.ToString(Args[1]);
end;

function FncUnusedName(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) > 0 then
    Result:=TLIL.AllocString(LIL.UnusedName(Args[0].StringValue))
  else
    Result:=TLIL.AllocString(LIL.UnusedName('unusedname'));
end;

function FncQuote(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i: Integer;
  s: string = '';
begin
  for i:=0 to Length(Args) - 1 do begin
    if i > 0 then s += ' ';
    s += TLIL.ToString(Args[i]);
  end;
  Result:=TLIL.AllocString(s);
end;

function FncSet(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i: Integer = 0;
  Variable: TLILVariable = nil;
  Locality: TLILSetVarLocality = lsvlLocal;
begin
  if Length(Args)=0 then exit(nil);
  if Args[0].Equals('global') then begin
    Locality:=lsvlGlobal;
    i:=1;
  end;
  while i < Length(Args) do begin
    if Length(Args)=i + 1 then exit(TLIL.Clone(LIL.GetVar(TLIL.ToString(Args[i]))));
    Variable:=LIL.SetVar(TLIL.ToString(Args[i]), Args[i + 1], Locality);
    Inc(i, 2);
  end;
  if Variable=nil then Result:=nil else Result:=TLIL.Clone(Variable.Value);
end;

function FncLocal(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i: Integer;
  VarName: string;
begin
  for i:=0 to Length(Args) - 1 do begin
    VarName:=TLIL.ToString(Args[i]);
    if not Assigned(LIL.FindLocalVar(LIL.Env, VarName)) then
      LIL.SetVar(VarName, LIL.Empty, lsvlLocalNew);
  end;
  Result:=nil;
end;

function FncWrite(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Msg: string = '';
  i: Integer;
begin
  for i:=0 to Length(Args) - 1 do begin
    if i > 0 then Msg += ' ';
    Msg += TLIL.ToString(Args[i]);
  end;
  if Assigned(LIL.FOnWrite) then
    LIL.FOnWrite(LIL, Msg)
  else
    Write(Msg);
  Result:=nil;
end;

function FncPrint(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  Result:=FncWrite(LIL, Args);
  if Assigned(LIL.FOnWrite) then
    LIL.FOnWrite(LIL, #10)
  else
    WriteLn;
end;

function FncEval(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Cmd: string;
  i: Integer;
begin
  if Length(Args)=1 then exit(LIL.ParseValue(Args[0]));
  if Length(Args) > 1 then begin
    Cmd:='';
    for i:=0 to Length(Args) - 1 do begin
      if i > 0 then Cmd += ' ';
      Cmd += TLIL.ToString(Args[i]);
    end;
    Result:=LIL.Parse(Cmd);
  end else
    Result:=nil;
end;

function FncTopEval(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  ThisEnv, ThisDownEnv: TLILEnvironment;
begin
  ThisEnv:=LIL.Env;
  ThisDownEnv:=LIL.DownEnv;
  LIL.Env:=LIL.RootEnv;
  LIL.DownEnv:=ThisEnv;
  Result:=FncEval(LIL, Args);
  LIL.Env:=ThisEnv;
  LIL.DownEnv:=ThisDownEnv;
end;

function FncUpEval(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  ThisEnv, ThisDownEnv: TLILEnvironment;
begin
  if LIL.RootEnv=LIL.Env then exit(FncEval(LIL, Args));
  ThisEnv:=LIL.Env;
  ThisDownEnv:=LIL.DownEnv;
  LIL.Env:=LIL.Env.Parent;
  LIL.DownEnv:=ThisEnv;
  Result:=FncEval(LIL, Args);
  LIL.Env:=ThisEnv;
  LIL.DownEnv:=ThisDownEnv;
end;

function FncDownEval(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  UpEnv, DownEnv: TLILEnvironment;
begin
  if LIL.DownEnv=nil then exit(FncEval(LIL, Args));
  UpEnv:=LIL.Env;
  DownEnv:=LIL.DownEnv;
  LIL.Env:=DownEnv;
  LIL.DownEnv:=nil;
  Result:=FncEval(LIL, Args);
  LIL.Env:=UpEnv;
  LIL.DownEnv:=DownEnv;
end;

function FncEnvEval(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  InVars: TLILList = nil;
  OutVars: TLILList = nil;
  VarValues: array of TLILValue;
  CodeIndex: Integer;
  i: Integer;
begin
  if Length(Args)=0 then Exit(nil)
  else if Length(Args)=1 then CodeIndex:=0
  else if Length(Args) >= 2 then begin
    InVars:=LIL.SubstituteToList(Args[0]);
    SetLength(VarValues, InVars.Count);
    for i:=0 to InVars.Count - 1 do
      VarValues[i]:=TLIL.Clone(LIL.GetVar(TLIL.ToString(InVars[i])));
    if Length(Args) > 2 then begin
      CodeIndex:=2;
      OutVars:=LIL.SubstituteToList(Args[1]);
    end else
      CodeIndex:=1;
  end;
  LIL.PushEnv;
  if Assigned(InVars) then
    for i:=0 to InVars.Count - 1 do begin
      LIL.SetVar(TLIL.ToString(InVars[i]), VarValues[i], lsvlLocalNew);
      FreeAndNil(VarValues[i]);
    end;
  Result:=LIL.ParseValue(Args[CodeIndex], False);
  if Assigned(InVars) or Assigned(OutVars) then
    if Assigned(OutVars) then
      for i:=0 to OutVars.Count - 1 do
        VarValues[i]:=TLIL.Clone(LIL.GetVar(TLIL.ToString(OutVars[i])))
    else
      for i:=0 to InVars.Count - 1 do
        VarValues[i]:=TLIL.Clone(LIL.GetVar(TLIL.ToString(InVars[i])));
  LIL.PopEnv;
  if Assigned(InVars) then begin
    if Assigned(OutVars) then
      for i:=0 to OutVars.Count - 1 do begin
        LIL.SetVar(TLIL.ToString(OutVars[i]), VarValues[i], lsvlLocal);
        FreeAndNil(VarValues[i]);
      end
    else
      for i:=0 to InVars.Count - 1 do begin
        LIL.SetVar(TLIL.ToString(InVars[i]), VarValues[i], lsvlLocal);
        FreeAndNil(VarValues[i]);
      end;
    FreeAndNil(InVars);
    FreeAndNil(OutVars);
  end;
end;

function FncJailEval(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Base, i: Integer;
  SubLIL: TLIL;
begin
  if Length(Args)=0 then exit(nil);
  if Args[0].Equals('clean') then begin
    Base:=1;
    if Length(Args)=1 then exit(nil);
  end else Base:=0;
  SubLIL:=TLIL.Create(LIL);
  if Base <> 1 then begin
    for i:=LIL.SysFuncs to Length(LIL.Funcs) - 1 do begin
      if LIL.Funcs[i].Native then
        SubLIL.Register(LIL.Funcs[i].Name, LIL.Funcs[i].Proc);
    end;
  end;
  Result:=SubLIL.ParseValue(Args[Base], True);
  FreeAndNil(SubLIL);
end;

function FncCount(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
begin
  if Length(Args)=0 then exit(TLIL.AllocString('0'));
  List:=LIL.SubstituteToList(Args[0]);
  Result:=TLIL.AllocInteger(List.Count);
  FreeAndNil(List);
end;

function FncIndex(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
  Index: Integer;
begin
  if Length(Args) < 2 then exit(nil);
  List:=LIL.SubstituteToList(Args[0]);
  Index:=TLIL.ToInteger(Args[1]);
  Result:=TLIL.Clone(List[Index]);
  FreeAndNil(List);
end;

function FncIndexOf(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
  i, Index: Integer;
begin
  if Length(Args) < 2 then exit(nil);
  List:=LIL.SubstituteToList(Args[0]);
  Index:=-1;
  for i:=0 to List.Count - 1 do
    if List[i].Equals(Args[1]) then begin
      Index:=i;
      break;
    end;
  Result:=TLIL.AllocInteger(Index);
  FreeAndNil(List);
end;

function FncFilter(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List, Filtered: TLILList;
  i: Integer;
  VarName: string = 'x';
  Base: Integer = 0;
begin
  Result:=nil;
  if Length(Args) < 1 then exit;
  if Length(Args) < 2 then exit(TLIL.Clone(Args[0]));
  if Length(Args) > 2 then begin
    Base:=1;
    VarName:=Args[0].StringValue;
  end;
  List:=LIL.SubstituteToList(Args[Base]);
  Filtered:=TLILList.Create;
  for i:=0 to List.Count - 1 do begin
    if LIL.Env.BreakRun then Break;
    LIL.SetVar(VarName, List[i], lsvlLocalOnly);
    Result:=LIL.EvaluateExpressionValue(Args[Base + 1]);
    if TLIL.ToBoolean(Result) then Filtered.Add(List[i].Clone);
    FreeAndNil(Result);
  end;
  FreeAndNil(List);
  Result:=Filtered.ToValue;
  FreeAndNil(Filtered);
end;

function FncAppend(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
  Base, i: Integer;
  VarName: string;
  Locality: TLILSetVarLocality = lsvlLocal;
begin
  if Length(Args) < 2 then exit(nil);
  Base:=1;
  VarName:=TLIL.ToString(Args[0]);
  if VarName='global' then begin
    if Length(Args) < 3 then exit(nil);
    VarName:=TLIL.ToString(Args[1]);
    Base:=2;
    Locality:=lsvlGlobal;
  end;
  List:=LIL.SubstituteToList(LIL.GetVar(VarName));
  for i:=Base to Length(Args) - 1 do
    List.Add(Args[i]);
  Result:=List.ToValue;
  FreeAndNil(List);
  LIL.SetVar(VarName, Result, Locality);
end;

function FncSlice(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List, Slice: TLILList;
  i, FromIndex, ToIndex: Integer;
begin
  if Length(Args) < 1 then exit(nil);
  if Length(Args) < 2 then exit(TLIL.Clone(Args[0]));
  FromIndex:=TLIL.ToInteger(Args[1]);
  if FromIndex < 0 then FromIndex:=0;
  List:=LIL.SubstituteToList(Args[0]);
  if Length(Args) > 2 then
    ToIndex:=TLIL.ToInteger(Args[2])
  else
    ToIndex:=List.Count;
  if ToIndex > List.Count then ToIndex:=List.Count;
  if ToIndex < FromIndex then ToIndex:=FromIndex;
  Slice:=TLILList.Create;
  for i:=FromIndex to ToIndex - 1 do
    Slice.Add(List[i]);
  FreeAndNil(List);
  Result:=Slice.ToValue;
  FreeAndNil(Slice);
end;

function FncList(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
  i: Integer;
begin
  List:=TLILList.Create;
  for i:=0 to Length(Args) - 1 do
    List.Add(Args[i]);
  Result:=List.ToValue;
  FreeAndNil(List);
end;

function FncSubst(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args)=0 then
    Result:=nil
  else
    Result:=LIL.SubstituteToValue(Args[0]);
end;

function FncConcat(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i: Integer;
  s: string = '';
  List: TLILList;
begin
  for i:=0 to Length(Args) - 1 do begin
    List:=LIL.SubstituteToList(Args[i]);
    Result:=List.ToValue;
    s += TLIL.ToString(Result);
    FreeAndNil(Result);
  end;
  Result:=TLIL.AllocString(s);
end;

function FncForEach(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i, ListIndex, CodeIndex: Integer;
  VarName: string = 'i';
  List, RetList: TLILList;
begin
  if Length(Args) < 2 then exit(nil);
  if Length(Args) >= 3 then begin
    VarName:=TLIL.ToString(Args[0]);
    ListIndex:=1;
    CodeIndex:=2;
  end else begin
    ListIndex:=0;
    CodeIndex:=1;
  end;
  RetList:=TLILList.Create;
  List:=LIL.SubstituteToList(Args[ListIndex]);
  for i:=0 to List.Count - 1 do begin
    if LIL.Env.BreakRun then Break;
    LIL.SetVar(VarName, List[i], lsvlLocalOnly);
    Result:=LIL.ParseValue(Args[CodeIndex]);
    if Result.Length > 0 then RetList.Add(Result);
    FreeAndNil(Result);
    if LIL.Error then break;
  end;
  Result:=RetList.ToValue;
  FreeAndNil(RetList);
  FreeAndNil(List);
end;

function FncReturn(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  LIL.Env.BreakRun:=True;
  FreeAndNil(LIL.Env.RetVal);
  if Length(Args) > 0 then begin
    LIL.Env.RetVal:=TLIL.Clone(Args[0]);
    Result:=TLIL.Clone(Args[0]);
  end else
    Result:=nil;
  LIL.Env.RetValSet:=True;
end;

function FncResult(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) > 0 then begin
    FreeAndNil(LIL.Env.RetVal);
    LIL.Env.RetVal:=TLIL.Clone(Args[0]);
    LIL.Env.RetValSet:=True;
  end;
  if LIL.Env.RetValSet then Result:=TLIL.Clone(LIL.Env.RetVal) else Result:=nil;
end;

function FncExpr(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i: Integer;
  Expr: TLILValue;
begin
  if Length(Args)=0 then exit(nil);
  if Length(Args)=1 then exit(LIL.EvaluateExpressionValue(Args[0]));
  Expr:=TLILValue.Create;
  for i:=0 to Length(Args) - 1 do begin
    if i > 0 then Expr.AppendChar(' ');
    Expr.AppendValue(Args[i]);
  end;
  Result:=LIL.EvaluateExpressionValue(Expr);
  FreeAndNil(Expr);
end;

function RealInc(LIL: TLIL; VarName: string; v: Extended): TLILValue;
var
  Value: Extended;
begin
  Result:=LIL.GetVar(VarName);
  Value:=LIL.ToFloat(Result) + v;
  if FMod(Value, 1) <> 0 then
    Result:=LIL.AllocFloat(Value)
  else
    Result:=LIL.AllocInteger(LIL.ToInteger(Result) + Trunc(v));
  LIL.SetVar(VarName, Result, lsvlLocal);
end;

function FncInc(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  v: Extended;
begin
  if Length(Args) < 1 then exit(nil);
  if Length(Args) > 1 then
    v:=TLIL.ToFloat(Args[1])
  else
    v:=1;
  Result:=RealInc(LIL, TLIL.ToString(Args[0]), v);
end;

function FncDec(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  v: Extended;
begin
  if Length(Args) < 1 then exit(nil);
  if Length(Args) > 1 then
    v:=-TLIL.ToFloat(Args[1])
  else
    v:=-1;
  Result:=RealInc(LIL, TLIL.ToString(Args[0]), v);
end;

function FncRead(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  f: file;
  Content: string;
  Buffer: PChar;
  Part: array [0..16384] of Char;
  Pos: Integer = 0;
  nr: Integer = 0;
begin
  if Length(Args) < 1 then exit(nil);
  if Assigned(LIL.FOnRead) then begin
    LIL.OnRead(LIL, TLIL.ToString(Args[0]), Content);
    Result:=TLIL.AllocString(Content);
  end else begin
    Assign(f, TLIL.ToString(Args[0]));
    {$I-}
    Reset(f, 1);
    {$I+}
    if IOResult <> 0 then exit(nil);
    Buffer:=GetMem(FileSize(f) + 1);
    while not Eof(f) do begin
      BlockRead(f, Part, SizeOf(Part), nr);
      if nr > 0 then begin
        Move(Part, Pointer(Buffer + Pos)^, nr);
        Inc(Pos, nr);
      end else break;
    end;
    Buffer[FileSize(f)]:=#0;
    Close(f);
    Result:=TLIL.AllocString(Buffer);
    FreeMem(Buffer);
  end;
end;

function FncStore(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  f: file;
  s: string;
begin
  if Length(Args) < 2 then exit(nil);
  if Assigned(LIL.FOnStore) then begin
    LIL.FOnStore(LIL, TLIL.ToString(Args[0]), TLIL.ToString(Args[1]));
  end else begin
    Assign(f, TLIL.ToString(Args[0]));
    {$I-}
    Rewrite(f, 1);
    {$I+}
    if IOResult <> 0 then exit(nil);
    s:=TLIL.ToString(Args[1]);
    BlockWrite(f, s[1], Length(s));
    Close(f);
  end;
  Result:=TLIL.Clone(Args[1]);
end;

function FncIf(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Value: TLILValue = nil;
  Base: Integer = 0;
  TruthNeeded: Boolean = True;
  v: Boolean;
begin
  Result:=nil;
  if Length(Args) < 1 then exit;
  if Args[0].Equals('not') then begin
    TruthNeeded:=False;
    Base:=1;
  end;
  if Length(Args) < Base + 2 then exit;
  Value:=LIL.EvaluateExpressionValue(Args[Base]);
  if LIL.Error or not Assigned(Value) then exit;
  v:=TLIL.ToBoolean(Value);
  if not TruthNeeded then v:=not v;
  if v then
    Result:=LIL.ParseValue(Args[Base + 1])
  else if Length(Args) > Base + 2 then
    Result:=LIL.ParseValue(Args[Base + 2]);
  FreeAndNil(Value);
end;

function FncWhile(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Value: TLILValue = nil;
  Base: Integer = 0;
  TruthNeeded: Boolean = True;
  v: Boolean;
begin
  Result:=nil;
  if Length(Args) < 1 then exit;
  if Args[0].Equals('not') then begin
    TruthNeeded:=False;
    Base:=1;
  end;
  if Length(Args) < Base + 2 then exit;
  while (not LIL.Error) and (not LIL.Env.BreakRun) do begin
    Value:=LIL.EvaluateExpressionValue(Args[Base]);
    if LIL.Error or not Assigned(Value) then exit;
    v:=TLIL.ToBoolean(Value);
    if not TruthNeeded then v:=not v;
    if not v then begin
      FreeAndNil(Value);
      break;
    end;
    if Assigned(Result) then FreeAndNil(Result);
    Result:=LIL.ParseValue(Args[Base + 1]);
    FreeAndNil(Value);
  end;
end;

function FncFor(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  Value: TLILValue;
begin
  Result:=nil;
  if Length(Args) < 4 then exit;
  Value:=LIL.ParseValue(Args[0]);
  FreeAndNil(Value);
  while (not LIL.Error) and (not LIL.Env.BreakRun) do begin
    Value:=LIL.EvaluateExpressionValue(Args[1]);
    if LIL.Error or not Assigned(Value) then exit;
    if not TLIL.ToBoolean(Value) then begin
      FreeAndNil(Value);
      break;
    end;
    FreeAndNil(Value);
    if Assigned(Result) then FreeAndNil(Result);
    Result:=LIL.ParseValue(Args[3]);
    Value:=LIL.ParseValue(Args[2]);
    FreeAndNil(Value);
  end;
end;

function FncChar(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 1 then exit(nil);
  Result:=TLIL.AllocString(Chr(Ord(TLIL.ToInteger(Args[0]))));
end;

function FncCharAt(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  s: string;
  Index: Integer;
begin
  if Length(Args) < 2 then exit(nil);
  s:=TLIL.ToString(Args[0]);
  Index:=TLIL.ToInteger(Args[1]);
  if (Index < 0) or (Index >= Length(s)) then exit(nil);
  Result:=TLIL.AllocString(s[Index + 1]);
end;

function FncCodeAt(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  s: string;
  Index: Integer;
begin
  if Length(Args) < 2 then exit(nil);
  s:=TLIL.ToString(Args[0]);
  Index:=TLIL.ToInteger(Args[1]);
  if (Index < 0) or (Index >= Length(s)) then exit(nil);
  Result:=TLIL.AllocInteger(Ord(s[Index + 1]));
end;

function FncSubStr(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  s: string;
  First, Last: Integer;
begin
  if Length(Args) < 2 then exit(nil);
  s:=TLIL.ToString(Args[0]);
  if s='' then exit(nil);
  First:=TLIL.ToInteger(Args[1]);
  if Length(Args) < 3 then
    Last:=Length(s)
  else
    Last:=TLIL.ToInteger(Args[2]);
  if Last > Length(s) then Last:=Length(s);
  if First >= Last then exit(nil);
  Result:=TLIL.AllocString(Copy(s, First + 1, Last - First));
end;

function FncStrPos(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  a, b: string;
begin
  if Length(Args) < 2 then exit(TLIL.AllocInteger(-1));
  a:=TLIL.ToString(Args[0]);
  b:=TLIL.ToString(Args[1]);
  Result:=TLIL.AllocInteger(Int64(Pos(b, a)) - 1);
end;

function FncLength(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  i, Total: Integer;
begin
  if Length(Args) < 1 then exit(TLIL.AllocInteger(0));
  Total:=0;
  for i:=0 to Length(Args) - 1 do begin
    if i > 0 then Inc(Total);
    Inc(Total, Length(TLIL.ToString(Args[i])));
  end;
  Result:=TLIL.AllocInteger(Total);
end;

function FncTrim(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 1 then exit(nil);
  Result:=TLIL.AllocString(Trim(TLIL.ToString(Args[0])));
end;

Function FncLTrim(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 1 then exit(nil);
  Result:=TLIL.AllocString(TrimLeft(TLIL.ToString(Args[0])));
end;

function FncRTrim(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 1 then exit(nil);
  Result:=TLIL.AllocString(TrimRight(TLIL.ToString(Args[0])));
end;

function FncStrCmp(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 2 then exit(nil);
  Result:=TLIL.AllocInteger(CompareStr(TLIL.ToString(Args[0]), TLIL.ToString(Args[1])));
end;

function FncStrEq(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 2 then exit(nil);
  if TLIL.ToString(Args[0])=TLIL.ToString(Args[1]) then
    Result:=TLIL.AllocInteger(1)
  else
    Result:=TLIL.AllocInteger(0);
end;

function FncRepStr(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 1 then exit(nil);
  if Length(Args) < 3 then exit(TLIL.Clone(Args[0]));
  Result:=TLIL.AllocString(StringReplace(TLIL.ToString(Args[0]), TLIL.ToString(Args[1]), TLIL.ToString(Args[2]), [rfReplaceAll]));
end;

function FncSplit(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
  Separators: string = ' ';
  i: Integer;
  Str, Part: string;
begin
  if Length(Args) < 1 then exit(nil);
  if Length(Args) > 1 then begin
    Separators:=TLIL.ToString(Args[1]);
    if Separators='' then exit(TLIL.Clone(Args[0]));
  end;
  Part:='';
  Str:=TLIL.ToString(Args[0]);
  List:=TLILList.Create;
  for i:=1 to Length(Str) do begin
    if Pos(Str[i], Separators) > 0 then begin
      List.AddString(Part);
      Part:='';
    end else
      Part += Str[i];
  end;
  List.AddString(Part);
  Result:=List.ToValue(True);
  FreeAndNil(List);
end;

function FncTry(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if (Length(Args) < 1) or (LIL.FError) then exit(nil);
  Result:=LIL.ParseValue(Args[0]);
  if LIL.FError then begin
    LIL.FError:=False;
    LIL.FErrorMessage:='';
    LIL.FErrorHead:=0;
    FreeAndNil(Result);
    if Length(Args) > 1 then Result:=LIL.ParseValue(Args[1]);
  end;
end;

function FncError(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) > 0 then
    LIL.SetError(TLIL.ToString(Args[0]))
  else
    LIL.SetError('Script initiated error');
  Result:=nil;
end;

function FncExit(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  ExitCode: Integer;
begin
  if Length(Args) > 0 then
    ExitCode:=TLIL.ToInteger(Args[0])
  else
    ExitCode:=0;
  if Assigned(LIL.FOnExit) then begin
    if Length(Args) > 0 then
      LIL.FOnExit(LIL, Args[0])
    else
      LIL.FOnExit(LIL, LIL.Empty);
  end else
    Halt(ExitCode);
  Result:=nil;
end;

function FncSource(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  f: file;
  Code: string;
  Buffer: PChar;
  Part: array [0..16384] of Char;
  Pos: Integer = 0;
  nr: Integer = 0;
begin
  if Length(Args) < 1 then exit(nil);
  if Assigned(LIL.FOnSource) then begin
    LIL.FOnSource(LIL, TLIL.ToString(Args[0]), Code);
    Result:=LIL.Parse(Code);
  end else if Assigned(LIL.FOnRead) then begin
    LIL.FOnRead(LIL, TLIL.ToString(Args[0]), Code);
    Result:=LIL.Parse(Code);
  end else begin
    Assign(f, TLIL.ToString(Args[0]));
    {$I-}
    Reset(f, 1);
    {$I+}
    if IOResult <> 0 then exit(nil);
    Buffer:=GetMem(FileSize(f) + 1);
    while not Eof(f) do begin
      BlockRead(f, Part, SizeOf(Part), nr);
      if nr > 0 then begin
        Move(Part, Pointer(Buffer + Pos)^, nr);
        Inc(Pos, nr);
      end else break;
    end;
    Buffer[FileSize(f)]:=#0;
    Close(f);
    Result:=LIL.Parse(Buffer);
    FreeMem(Buffer);
  end;
end;

function FncLMap(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
var
  List: TLILList;
  i: Integer;
begin
  Result:=nil;
  if Length(Args) < 2 then exit;
  List:=LIL.SubstituteToList(Args[0]);
  for i:=1 to Length(Args) - 1 do
    LIL.SetVar(TLIL.ToString(Args[i]), List[i - 1], lsvlLocal);
  FreeAndNil(List);
end;

function FncRand(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  Result:=TLIL.AllocFloat(Random);
end;

function FncCatcher(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
begin
  if Length(Args) < 1 then begin
    Result:=TLIL.AllocString(LIL.Catcher);
  end else begin
    LIL.Catcher:=TLIL.ToString(Args[0]);
    Result:=nil;
  end;
end;

procedure Register;
begin
  RegisterComponents('Misc', [TLIL]);
end;

{ TLILValue }
function TLILValue.ToInteger: Int64;
begin
  try
    Result:=StrToInt64(FData);
  except
    try
      Result:=Trunc(StrToFloat(FData));
    except
      Result:=0;
    end;
  end;
end;

function TLILValue.ToFloat: Extended;
begin
  try
    Result:=StrToFloat(FData, LILFormatSettings);
  except
    Result:=0;
  end;
end;

function TLILValue.ToBoolean: Boolean;
var
  i: Integer;
  Dots: Boolean = False;
begin
  if FData='' then exit(false);
  for i:=1 to FLength do begin
    if not (FData[i] in ['0', '.']) then exit(true);
    if FData[i]='.' then begin
      if Dots then exit(true);
      Dots:=True;
    end;
  end;
  Result:=false;
end;

procedure TLILValue.AppendChar(Ch: Char);
begin
  FData += Ch;
  Inc(FLength);
end;

procedure TLILValue.AppendString(Str: string);
begin
  FData += Str;
  Inc(FLength, System.Length(Str));
end;

procedure TLILValue.AppendValue(AValue: TLILValue);
begin
  if Assigned(AValue) then begin
    FData += AValue.FData;
    Inc(FLength, AValue.FLength);
  end;
end;

function TLILValue.Clone: TLILValue;
begin
  Result:=TLILValue.Create;
  Result.FData:=FData;
  Result.FLength:=FLength;
end;

function TLILValue.Equals(StrVal: string): Boolean;
begin
  Result:=StrVal=FData;
end;

function TLILValue.Equals(IntVal: Int64): Boolean;
begin
  Result:=IntVal=ToInteger;
end;

function TLILValue.Equals(FloatVal: Extended): Boolean;
begin
  Result:=FloatVal=ToFloat;
end;

function TLILValue.Equals(LILVal: TLILValue): Boolean;
begin
  Result:=(LILVal <> nil) and (LILVal.FData=FData);
end;

{ TLILVariable }
procedure TLILVariable.SetValue(AValue: TLILValue);
var
  OldValue: TLILValue;
begin
  OldValue:=FValue;
  FValue:=TLIL.Clone(AValue);
  FreeAndNil(OldValue);
end;

constructor TLILVariable.Create(AName: string; AEnvironment: TLILEnvironment);
begin
  inherited Create;
  FName:=AName;
  FEnvironment:=AEnvironment;
end;

destructor TLILVariable.Destroy;
begin
  FreeAndNil(FValue);
  inherited Destroy;
end;

{ TLILEnvironment }
constructor TLILEnvironment.Create(AParent: TLILEnvironment);
begin
  inherited Create;
  Parent:=AParent;
  Vars:=TFPList.Create;
end;

destructor TLILEnvironment.Destroy;
var
  i: Integer;
begin
  FreeAndNil(RetVal);
  for i:=0 to Vars.Count - 1 do TLILVariable(Vars[i]).Free;
  FreeAndNil(Vars);
  inherited Destroy;
end;

procedure TLILEnvironment.RegisterVariable(AVariable: TLILVariable);
begin
  Vars.Add(AVariable);
end;

{ TLILList }
function TLILList.GetCount: Integer;
begin
  Result:=FValues.Count;
end;

function TLILList.GetValue(AIndex: Integer): TLILValue;
begin
  if (AIndex < 0) or (AIndex >= FValues.Count) then
    Result:=nil
  else
    Result:=TLILValue(FValues[AIndex]);
end;

constructor TLILList.Create;
begin
  inherited Create;
  FValues:=TFPList.Create;
end;

destructor TLILList.Destroy;
var
  i: Integer;
begin
  for i:=0 to FValues.Count - 1 do TLILValue(FValues[i]).Free;
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TLILList.Add(AValue: TLILValue);
begin
  FValues.Add(TLIL.Clone(AValue));
end;

procedure TLILList.AddString(AString: string);
begin
  FValues.Add(TLIL.AllocString(AString));
end;

procedure TLILList.AddInteger(AInteger: Int64);
begin
  FValues.Add(TLIL.AllocInteger(AInteger));
end;

procedure TLILList.AddFloat(AFloat: Extended);
begin
  FValues.Add(TLIL.AllocFloat(AFloat));
end;

procedure TLILList.AddBoolean(ABoolean: Boolean);
begin
  FValues.Add(TLIL.AllocBoolean(ABoolean));
end;

function TLILList.IndexOf(AValue: TLILValue): Integer;
var
  i: Integer;
begin
  for i:=0 to Count - 1 do if AValue.Equals(Values[i]) then Exit(i);
  Result:=-1;
end;

function TLILList.IndexOfString(AString: string): Integer;
var
  i: Integer;
begin
  for i:=0 to Count - 1 do if AString=TLIL.ToString(Values[i]) then Exit(i);
  Result:=-1;
end;

function TLILList.IndexOfInteger(AInteger: Int64): Integer;
var
  i: Integer;
begin
  for i:=0 to Count - 1 do if AInteger=TLIL.ToInteger(Values[i]) then Exit(i);
  Result:=-1;
end;

function TLILList.IndexOfFloat(AFloat: Extended): Integer;
var
  i: Integer;
begin
  for i:=0 to Count - 1 do if AFloat=TLIL.ToFloat(Values[i]) then Exit(i);
  Result:=-1;
end;

function TLILList.IndexOfBoolean(ABoolean: Boolean): Integer;
var
  i: Integer;
begin
  for i:=0 to Count - 1 do if ABoolean=TLIL.ToBoolean(Values[i]) then Exit(i);
  Result:=-1;
end;

function TLILList.Has(AValue: TLILValue): Boolean;
begin
  Result:=IndexOf(AValue) <> -1;
end;

function TLILList.HasString(AString: string): Boolean;
begin
  Result:=IndexOfString(AString) <> -1;
end;

function TLILList.HasInteger(AInteger: Int64): Boolean;
begin
  Result:=IndexOfInteger(AInteger) <> -1;
end;

function TLILList.HasFloat(AFloat: Extended): Boolean;
begin
  Result:=IndexOfFloat(AFloat) <> -1;
end;

function TLILList.HasBoolean(ABoolean: Boolean): Boolean;
begin
  Result:=IndexOfBoolean(ABoolean) <> -1;
end;

function TLILList.ToValue(DoEscape: Boolean=True): TLILValue;
var
  i: Integer;
  Escape: Boolean;
  Value: TLILValue;

  function NeedsEscape(s: string): Boolean;
  var
    i: Integer;
  begin
    if s='' then exit(True);
    for i:=1 to Length(s) do
      if (IsPunct(s[i]) or IsSpace(s[i])) then exit(True);
    Result:=False;
  end;

begin
  Result:=TLILValue.Create;
  for i:=0 to FValues.Count - 1 do begin
    Value:=TLILValue(FValues[i]);
    Escape:=DoEscape and NeedsEscape(TLIL.ToString(Value));
    if i > 0 then Result.AppendChar(' ');
    if Escape then Result.AppendChar('{');
    Result.AppendValue(Value);
    if Escape then Result.AppendChar('}');
  end;
end;

function TLILList.ToFunctionArgs: TLILFunctionProcArgs;
var
  i: Integer;
begin
  SetLength(Result, FValues.Count - 1);
  for i:=1 to FValues.Count - 1 do Result[i - 1]:=TLILValue(FValues[i]);
end;

{ TLILFunction }
constructor TLILFunction.Create;
begin
  inherited Create;
  ArgNames:=TLILList.Create;
end;

destructor TLILFunction.Destroy;
begin
  FreeAndNil(ArgNames);
  inherited Destroy;
end;

function TLILFunction.GetBody: string;
begin
  Result:=TLIL.ToString(Code);
end;

function TLILFunction.IsNative: Boolean;
begin
  Result:=Assigned(Proc);
end;

function TLILFunction.GetArguments: Integer;
begin
  if Assigned(ArgNames) then
    Result:=ArgNames.Count
  else
    Result:=0;
end;

function TLILFunction.GetArgument(AIndex: Integer): string;
begin
  if Assigned(ArgNames) and (AIndex >= 0) and (AIndex < ArgNames.Count) then
    Result:=TLIL.ToString(ArgNames[AIndex])
  else
    Result:='';
end;

{ TLIL }
procedure TLIL.RegisterStandardFunctions;
begin
  Register('reflect', @FncReflect);
  Register('func', @FncFunc);
  Register('rename', @FncRename);
  Register('unusedname', @FncUnusedName);
  Register('quote', @FncQuote);
  Register('set', @FncSet);
  Register('local', @FncLocal);
  Register('write', @FncWrite);
  Register('print', @FncPrint);
  Register('eval', @FncEval);
  Register('topeval', @FncTopEval);
  Register('upeval', @FncUpEval);
  Register('downeval', @FncDownEval);
  Register('enveval', @FncEnvEval);
  Register('jaileval', @FncJailEval);
  Register('count', @FncCount);
  Register('index', @FncIndex);
  Register('indexof', @FncIndexOf);
  Register('filter', @FncFilter);
  Register('append', @FncAppend);
  Register('slice', @FncSlice);
  Register('list', @FncList);
  Register('subst', @FncSubst);
  Register('concat', @FncConcat);
  Register('foreach', @FncForeach);
  Register('return', @FncReturn);
  Register('result', @FncResult);
  Register('expr', @FncExpr);
  Register('inc', @FncInc);
  Register('dec', @FncDec);
  Register('read', @FncRead);
  Register('store', @FncStore);
  Register('if', @FncIf);
  Register('while', @FncWhile);
  Register('for', @FncFor);
  Register('char', @FncChar);
  Register('charat', @FncCharAt);
  Register('codeat', @FncCodeAt);
  Register('substr', @FncSubStr);
  Register('strpos', @FncStrPos);
  Register('length', @FncLength);
  Register('trim', @FncTrim);
  Register('ltrim', @FncLTrim);
  Register('rtrim', @FncRTrim);
  Register('strcmp', @FncStrCmp);
  Register('streq', @FncStrEq);
  Register('repstr', @FncRepStr);
  Register('split', @FncSplit);
  Register('try', @FncTry);
  Register('error', @FncError);
  Register('exit', @FncExit);
  Register('source', @FncSource);
  Register('lmap', @FncLMap);
  Register('rand', @FncRand);
  Register('catcher', @FncCatcher);
  SysFuncs:=Length(Funcs);
end;

function TLIL.FindLocalVar(AEnv: TLILEnvironment; AName: string): TLILVariable;
var
  i: Integer;
begin
  for i:=AEnv.Vars.Count - 1 downto 0 do
    if TLILVariable(AEnv.Vars[i]).Name=AName then
      exit(TLILVariable(AEnv.Vars[i]));
  Result:=nil;
end;

function TLIL.FindVar(AEnv: TLILEnvironment; AName: string): TLILVariable;
begin
  Result:=FindLocalVar(AEnv, AName);
  if not Assigned(Result) and (AEnv <> RootEnv) then Result:=FindVar(RootEnv, AName);
end;

function TLIL.FindFunction(AName: string): TLILFunction;
var
  i: Integer;
begin
  for i:=Length(Funcs) - 1 downto 0 do
    if Funcs[i].Name=AName then
      exit(Funcs[i]);
  Result:=nil;
end;

function TLIL.AddFunction(AName: string): TLILFunction;
begin
  Result:=FindFunction(AName);
  if Result <> nil then exit(Result);
  Result:=TLILFunction.Create;
  Result.FName:=AName;
  SetLength(Funcs, Length(Funcs) + 1);
  Funcs[Length(Funcs) - 1]:=Result;
end;

procedure TLIL.Register(AName: string; AProc: TLILFunctionProc);
var
  Func: TLILFunction;
begin
  Func:=AddFunction(AName);
  Func.Proc:=AProc;
end;

function TLIL.GetFunction(AName: string): TLILFunction;
begin
  Result:=FindFunction(AName);
end;

function TLIL.AtEol: Boolean;
begin
  Result:=(Head <= CLen) and (Code[Head] in [#10, #13, ';']);
end;

procedure TLIL.SkipSpaces;
begin
  while (Head <= CLen) and (Code[Head] in ['\', '#', #9, #11, #12, #32]) do begin
    if Code[Head]='#' then begin
      if (Head < CLen) and (Code[Head + 1]='#') then begin
        Inc(Head, 2);
        while Head <= CLen do begin
          if (Code[Head]='#') and (Head < CLen) and (Code[Head + 1]='#') then begin
            Inc(Head, 2);
            Break;
          end;
          Inc(Head);
        end;
      end else begin
        while (Head <= CLen) and (not AtEol) do Inc(Head);
      end;
    end else if (Code[Head]='\') and (Code[Head + 1] in [#10, #13]) then begin
      Inc(Head);
      while (Head <= CLen) and AtEol do Inc(Head);
    end else
      Inc(Head);
  end;
end;

function TLIL.GetBracketPart: TLILValue;
var
  Counter: Integer = 1;
  Command: string = '';
begin
  Inc(Head);
  while Head <= CLen do begin
    if Code[Head]='[' then begin
      Inc(Head);
      Inc(Counter);
      Command += '[';
    end else if Code[Head]=']' then begin
      Inc(Head);
      Dec(Counter);
      if Counter=0 then break;
      Command += ']';
    end else begin
      Command += Code[Head];
      Inc(Head);
    end;
  end;
  Result:=Parse(Command);
end;

function TLIL.GetDollarPart: TLILValue;
var
  TheNextWord: TLILValue;
  Command: string;
begin
  Inc(Head);
  TheNextWord:=NextWord;
  Command:=DollarPrefix + ToString(TheNextWord);
  FreeAndNil(TheNextWord);
  Result:=Parse(Command);
end;

function TLIL.NextWord: TLILValue;
var
  Counter: Integer;
  Str: string;
  Sc: Char;
  Temp: TLILValue;
begin
  Result:=nil;
  SkipSpaces;
  if Head > CLen then exit;
  if Code[Head]='$' then begin
    Result:=GetDollarPart;
  end else if Code[Head]='{' then begin
    Counter:=1;
    Inc(Head);
    Str:='';
    while Head <= CLen do begin
      if Code[Head]='{' then begin
        Inc(Head);
        Inc(Counter);
        Str += '{';
      end else if Code[Head]='}' then begin
        Inc(Head);
        Dec(Counter);
        if Counter=0 then break;
        Str += '}';
      end else begin
        Str += Code[Head];
        Inc(Head);
      end;
    end;
    Result:=TLIL.AllocString(Str);
  end else if Code[Head]='[' then begin
    Result:=GetBracketPart;
  end else if Code[Head] in ['"', ''''] then begin
    Result:=TLILValue.Create;
    Sc:=Code[Head];
    Inc(Head);
    while Head <= CLen do begin
      if Code[Head]='[' then begin
        Temp:=GetBracketPart;
        Result.AppendValue(Temp);
        FreeAndNil(Temp);
        Dec(Head); // Avoid skipping the character at the end of 'while'
      end else if Code[Head]='$' then begin
        Temp:=GetDollarPart;
        Result.AppendValue(Temp);
        FreeAndNil(Temp);
        Dec(Head); // Avoid skipping the character at the end of 'while'
      end else if Code[Head]='\' then begin
        Inc(Head);
        case Code[Head] of
          'b': Result.AppendChar(#8);
          't': Result.AppendChar(#9);
          'n': Result.AppendChar(#10);
          'v': Result.AppendChar(#11);
          'f': Result.AppendChar(#12);
          'r': Result.AppendChar(#13);
          '0': Result.AppendChar(#0);
          'a': Result.AppendChar(#7);
          'c': Result.AppendChar('}');
          'o': Result.AppendChar('{');
          else Result.AppendChar(Code[Head]);
        end;
      end else if Code[Head]=Sc then begin
        Inc(Head);
        break;
      end else begin
        Result.AppendChar(Code[Head]);
      end;
      Inc(Head);
    end;
  end else begin
    Str:='';
    while (Head <= Clen) and (not IsSpace(Code[Head])) and (not IsSpecial(Code[Head])) do begin
      Str += Code[Head];
      Inc(Head);
    end;
    Result:=TLIL.AllocString(Str);
  end;
  if Result=nil then Result:=TLILValue.Create;
end;

function TLIL.Substitute: TLILList;
var
  W, WP: TLILValue;
  SHead: Integer;
begin
  Result:=TLILList.Create;
  SkipSpaces;
  while (Head <= CLen) and (not AtEol) and (not FError) do begin
    W:=TLILValue.Create;
    repeat
      SHead:=Head;
      WP:=NextWord;
      if SHead=Head then begin { something went wrong, the parser can't proceed }
        FreeAndNil(W);
        FreeAndNil(WP);
        FreeAndNil(Result);
        exit;
      end;
      W.AppendValue(WP);
      FreeAndNil(WP);
    until (Head > CLen) or AtEol or IsSpace(Code[Head]) or FError;
    SkipSpaces;
    Result.Add(W);
    FreeAndNil(W);
  end;
end;

constructor TLIL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RootEnv:=TLILEnvironment.Create(nil);
  Empty:=TLILValue.Create;
  DollarPrefix:='set ';
  RegisterStandardFunctions;
end;

destructor TLIL.Destroy;
var
  i: Integer;
begin
  for i:=0 to Length(Funcs) - 1 do Funcs[i].Free;
  FreeAndNil(RootEnv);
  FreeAndNil(Empty);
  inherited Destroy;
end;

class function TLIL.AllocString(Str: string): TLILValue;
begin
  Result:=TLILValue.Create;
  Result.FData:=Str;
  Result.FLength:=System.Length(Str);
end;

class function TLIL.AllocInteger(Int: Int64): TLILValue;
begin
  Result:=AllocString(IntToStr(Int));
end;

class function TLIL.AllocFloat(Float: Extended): TLILValue;
begin
  if Trunc(Float)=Float then Result:=AllocString(IntToStr(Trunc(Float))+'.0') else Result:=AllocString(FloatToStrF(Float, ffFixed, 15, 10, LILFormatSettings));
end;

class function TLIL.AllocBoolean(Bool: Boolean): TLILValue;
begin
  if Bool then Result:=AllocString('1') else Result:=AllocString('');
end;

class function TLIL.ToString(AValue: TLILValue): string;
begin
  if AValue=nil then Result:='' else Result:=AValue.StringValue;
end;

class function TLIL.ToInteger(AValue: TLILValue): Int64;
begin
  if AValue=nil then Result:=0 else Result:=AValue.IntegerValue;
end;

class function TLIL.ToFloat(AValue: TLILValue): Extended;
begin
  if AValue=nil then Result:=0 else Result:=AValue.FloatValue;
end;

class function TLIL.ToBoolean(AValue: TLILValue): Boolean;
begin
  if AValue=nil then Result:=False else Result:=AValue.BooleanValue;
end;

class function TLIL.Clone(AValue: TLILValue): TLILValue;
begin
  if AValue=nil then Result:=nil else Result:=AValue.Clone;
end;

procedure TLIL.SetError(AErrorMessage: string);
begin
  if not FError then begin
    FError:=True;
    FErrorHead:=Head;
    FErrorMessage:=AErrorMessage;
  end;
end;

function TLIL.SetVar(AName: string; AValue: TLILValue; Locality: TLILSetVarLocality): TLILVariable;
var
  TargetEnv: TLILEnvironment;
  Variable: TLILVariable;
  SetVarAction: TLILOnSetVarAction = losaDefault;
  Replacement: TLILValue = nil;
  FreeValue: Boolean = False;
begin
  Result:=nil;
  if AName='' then exit;
  if Locality=lsvlGlobal then TargetEnv:=RootEnv else TargetEnv:=Env;
  if Locality <> lsvlLocalNew then begin
    Variable:=FindVar(TargetEnv, AName);
    if (Locality=lsvlLocalOnly) and Assigned(Variable) and
       (Variable.Environment=RootEnv) and (Variable.Environment <> TargetEnv) then Variable:=nil;
    if Assigned(FOnSetVar) and (Env=RootEnv) then begin
      FOnSetVar(Self, AName, AValue, Replacement, SetVarAction);
      case SetVarAction of
        losaIgnore: exit;
        losaReplace: begin
          AValue:=Replacement;
          FreeValue:=True;
        end;
      end;
    end;
    if Variable <> nil then begin
      Variable.Value:=AValue;
      Result:=Variable;
      if FreeValue then FreeAndNil(AValue);
      exit;
    end;
  end;

  Result:=TLILVariable.Create(AName, TargetEnv);
  Result.Value:=AValue;
  TargetEnv.RegisterVariable(Result);
  if FreeValue then FreeAndNil(AValue);
end;

function TLIL.GetVar(AName: string; DefaultValue: TLILValue=nil): TLILValue;
var
  Variable: TLILVariable;
  Replacement: TLILValue = nil;
  GetVarAction: TLILOnGetVarAction = logaDefault;
begin
  Variable:=FindVar(Env, AName);
  if Variable=nil then Result:=DefaultValue else Result:=Variable.Value;
  if Assigned(FOnGetVar) then begin
    FOnGetVar(Self, AName, Result, Replacement, GetVarAction);
    if GetVarAction=logaReplace then begin
      Result:=Replacement;
    end;
  end;
end;

function TLIL.UnusedName(Part: string): string;
var
  i: Integer;
begin
  for i:=0 to MaxInt do begin
    Result:='!!un!' + Part + '!' + IntToStr(i) + '!nu!!';
    if (FindFunction(Result) <> nil) or (FindVar(Env, Result) <> nil) then continue;
    exit;
  end;
  Result:='';
end;

procedure TLIL.PushEnv;
begin
  Env:=TLILEnvironment.Create(Env);
end;

procedure TLIL.PopEnv;
var
  Next: TLILEnvironment;
begin
  if Env.Parent=nil then exit;
  Next:=Env.Parent;
  Env.Free;
  Env:=Next;
end;

function TLIL.SubstituteToList(ACode: TLILValue): TLILList;
var
  SaveCode: string;
  SaveHead, SaveCLen: Integer;
begin
  SaveCode:=Code;
  SaveHead:=Head;
  SaveCLen:=CLen;
  Code:=TLIL.ToString(ACode);
  CLen:=Length(Code);
  Head:=1;
  Result:=Substitute;
  Head:=SaveHead;
  CLen:=SaveCLen;
  Code:=SaveCode;
end;

function TLIL.SubstituteToValue(ACode: TLILValue): TLILValue;
var
  Words: TLILList;
begin
  Words:=SubstituteToList(ACode);
  if Words=nil then exit(TLIL.Clone(ACode));
  Result:=Words.ToValue(False);
  FreeAndNil(Words);
end;

function TLIL.EvaluateExpression(ACode: string): TLILValue;
var
  Temp: TLILValue;
begin
  Temp:=TLIL.AllocString(ACode);
  Result:=EvaluateExpressionValue(Temp);
  FreeAndNil(Temp);
end;

function LIL_EvaluateExpressionValue(LIL: TLIL; AValue: TLILValue): TLILValue;
type
  TExprEvalType = (eeInt, eeFloat);
  TExprEvalError = (eeNoError, eeSyntaxError, eeInvalidType, eeDivisionByZero, eeInvalidExpression);
var
  Code: string;
  Len, Head: Integer;
  IVal: Int64;
  FVal: Extended;
  XType: TExprEvalType;
  Error: TExprEvalError;

  procedure SubExpression; forward;

  function InvalidPunctuation(Ch: Char): Boolean; inline;
  begin
    Result:=IsPunct(Ch) and not (Ch in ['!', '~', '(', ')', '-', '+']);
  end;

  procedure SkipSpaces; inline;
  begin
    while (Head <= Len) and IsSpace(Code[Head]) do Inc(Head);
  end;

  procedure NumericElement;
  var
    FPart: Int64;
    FPartLen: Int64;
  begin
    XType:=eeInt;
    IVal:=0;
    FVal:=0;
    SkipSpaces;
    FPart:=0;
    FPartLen:=1;
    while (Head <= Len) do begin
      if Code[Head]='.' then begin
        if XType=eeFloat then break;
        XType:=eeFloat;
        Inc(Head);
        continue;
      end else if not IsDigit(Code[Head]) then break;
      if XType=eeInt then
        IVal:=IVal*10 + (Int64(Ord(Code[Head])) - Ord('0'))
      else begin
        FPart:=FPart*10 + (Int64(Ord(Code[Head])) - Ord('0'));
        FPartLen:=FPartLen*10;
      end;
      Inc(Head);
    end;
    if XType=eeFloat then FVal:=IVal + FPart/FPartLen;
  end;

  procedure Element;
  begin
    SkipSpaces;
    if (Head <= Len) and IsDigit(Code[Head]) then begin
      NumericElement;
    end else begin
      // Assume that anything else is a string that was used to evaluate
      // as a "true" value.
      XType:=eeInt;
      IVal:=1;
      Error:=eeInvalidExpression; // this will be cleared
    end;
  end;

  procedure Paren;
  begin
    SkipSpaces;
    if Head > Len then exit;
    if Code[Head]='(' then begin
      Inc(Head);
      SubExpression;
      SkipSpaces;
      if (Head <= Len) and (Code[Head]=')') then
        Inc(Head)
      else
        Error:=eeSyntaxError;
    end else begin
      Element;
    end;
  end;

  procedure Unary;
  var
    Op: Char;
  begin
    SkipSpaces;
    if (Head < Len) and (Error=eeNoError) and (Code[Head] in ['-', '+', '~', '!']) then begin
      Op:=Code[Head];
      Inc(Head);
      Unary;
      if Error <> eeNoError then exit;
      case Op of
        '-': case XType of
          eeFloat: FVal:=-FVal;
          eeInt: IVal:=-IVal;
          else Error:=eeInvalidType;
        end;
        // Ignore '+'
        '~': case XType of
          eeFloat: FVal:=not Trunc(FVal);
          eeInt: IVal:=not IVal;
          else Error:=eeInvalidType;
        end;
        '!': case XType of
          eeFloat: if FVal=0 then FVal:=1 else FVal:=0;
          eeInt: if IVal=0 then IVal:=1 else IVal:=0;
          else Error:=eeInvalidType;
        end;
      end;
    end else begin
      Paren;
    end;
  end;

  procedure MulDiv;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    Unary;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and (not InvalidPunctuation(Code[Head + 1])) and (Code[Head] in ['*', '/', '\', '%']) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      case Code[Head] of
        '*': case XType of
          eeFloat: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: FVal:=OFVal*FVal;
              eeInt: begin
                FVal:=OFVal*IVal;
                XType:=eeFloat;
              end;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: FVal:=OIVal*FVal;
              eeInt: IVal:=OIVal*IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        '%': case XType of
          eeFloat: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: if FVal=0 then
                  Error:=eeDivisionByZero
                else
                  FVal:=FMod(OFVal, FVal);
              eeInt: if IVal=0 then
                  Error:=eeDivisionByZero
                else begin
                  FVal:=FMod(OFVal, IVal);
                  XType:=eeFloat;
                end;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: if FVal=0 then
                  Error:=eeDivisionByZero
                else
                  FVal:=FMod(OIVal, FVal);
              eeInt: if IVal=0 then
                  Error:=eeDivisionByZero
                else
                  IVal:=OIVal mod IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        '/': case XType of
          eeFloat: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: if FVal=0 then
                  Error:=eeDivisionByZero
                else
                  FVal:=OFVal/FVal;
              eeInt: if IVal=0 then
                  Error:=eeDivisionByZero
                else begin
                  FVal:=OFVal/IVal;
                  XType:=eeFloat;
                end;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: if FVal=0 then
                  Error:=eeDivisionByZero
                else
                  FVal:=OIVal/FVal;
              eeInt: if IVal=0 then
                  Error:=eeDivisionByZero
                else
                  IVal:=OIVal div IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        '\': case XType of
          eeFloat: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: if FVal=0 then
                  Error:=eeDivisionByZero
                else begin
                  IVal:=Trunc(OFVal/FVal);
                  XType:=eeInt;
                end;
              eeInt: if IVal=0 then
                  Error:=eeDivisionByZero
                else
                  IVal:=Trunc(OFVal/IVal);
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Unary;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: if FVal=0 then
                  Error:=eeDivisionByZero
                else begin
                  FVal:=Trunc(OIVal/FVal);
                  XType:=eeFloat;
                end;
              eeInt: if IVal=0 then
                  Error:=eeDivisionByZero
                else
                  IVal:=OIVal div IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
      end;
      SkipSpaces;
    end;
  end;

  procedure AddSub;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    MulDiv;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and (not InvalidPunctuation(Code[Head + 1])) and (Code[Head] in ['+', '-']) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      case Code[Head] of
        '+': case XType of
          eeFloat: begin
            Inc(Head);
            MulDiv;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: FVal:=OFVal + FVal;
              eeInt: begin
                FVal:=OFVal + IVal;
                XType:=eeFloat;
              end;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            MulDiv;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: FVal:=OIVal + FVal;
              eeInt: IVal:=OIVal + IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        '-': case XType of
          eeFloat: begin
            Inc(Head);
            MulDiv;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: FVal:=OFVal - FVal;
              eeInt: begin
                FVal:=OFVal - IVal;
                XType:=eeFloat;
              end;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            MulDiv;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: FVal:=OIVal - FVal;
              eeInt: IVal:=OIVal - IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
      end;
      SkipSpaces;
    end;
  end;

  procedure Shift;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    AddSub;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and (
      ((Code[Head]='>') and (Code[Head + 1]='>')) or
      ((Code[Head]='<') and (Code[Head + 1]='<'))
    ) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      Inc(Head);
      case Code[Head] of
        '<': case XType of
          eeFloat: begin
            Inc(Head);
            AddSub;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                IVal:=Trunc(OFVal) shl Trunc(FVal);
                XType:=eeInt;
              end;
              eeInt: IVal:=Trunc(OFVal) shl IVal;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            AddSub;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                IVal:=OIVal shl Trunc(FVal);
                XType:=eeInt;
              end;
              eeInt: IVal:=OIVal shl IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        '>': case XType of
          eeFloat: begin
            Inc(Head);
            AddSub;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                IVal:=Trunc(OFVal) shr Trunc(FVal);
                XType:=eeInt;
              end;
              eeInt: IVal:=Trunc(OFVal) shr IVal;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            AddSub;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                IVal:=OIVal shr Trunc(FVal);
                XType:=eeInt;
              end;
              eeInt: IVal:=OIVal shr IVal;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
      end;
      SkipSpaces;
    end;
  end;

  procedure Compare;
  var
    OFVal: Extended;
    OIVal: Int64;
    Op: Integer;
  begin
    Shift;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and (
      ((Code[Head]='<') and (not InvalidPunctuation(Code[Head + 1]))) or
      ((Code[Head]='>') and (not InvalidPunctuation(Code[Head + 1]))) or
      ((Code[Head]='<') and (Code[Head + 1]='=')) or
      ((Code[Head]='>') and (Code[Head + 1]='='))
    ) do begin
      Op:=4;
      if (Code[Head]='<') and (not InvalidPunctuation(Code[Head + 1])) then Op:=1 else
      if (Code[Head]='>') and (not InvalidPunctuation(Code[Head + 1])) then Op:=2 else
      if (Code[Head]='<') and (Code[Head + 1]='=') then Op:=3;
      if Op > 2 then Inc(Head, 2) else Inc(Head);
      OFVal:=FVal;
      OIVal:=IVal;
      case Op of
        1: case XType of
          eeFloat: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OFVal < FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OFVal < IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OIVal < FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OIVal < IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        2: case XType of
          eeFloat: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OFVal > FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OFVal > IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OIVal > FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OIVal > IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        3: case XType of
          eeFloat: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OFVal <= FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OFVal <= IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OIVal <= FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OIVal <= IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        4: case XType of
          eeFloat: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OFVal >= FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OFVal >= IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Shift;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OIVal >= FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OIVal >= IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
      end;
      SkipSpaces;
    end;
  end;

  procedure Equals;
  var
    OFVal: Extended;
    OIVal: Int64;
    Op: Integer;
  begin
    Compare;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and (
      ((Code[Head]='=') and (Code[Head + 1]='=')) or
      ((Code[Head]='!') and (Code[Head + 1]='='))
    ) do begin
      if (Code[Head]='=') then Op:=1 else Op:=2;
      Inc(Head);
      OFVal:=FVal;
      OIVal:=IVal;
      case Op of
        1: case XType of
          eeFloat: begin
            Inc(Head);
            Compare;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OFVal=FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OFVal=IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Compare;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OIVal=FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OIVal=IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
        2: case XType of
          eeFloat: begin
            Inc(Head);
            Compare;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OFVal <> FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OFVal <> IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          eeInt: begin
            Inc(Head);
            Compare;
            if Error <> eeNoError then exit;
            case XType of
              eeFloat: begin
                if OIVal <> FVal then IVal:=1 else IVal:=0;
                XType:=eeInt;
              end;
              eeInt: if OIVal <> IVal then IVal:=1 else IVal:=0;
              else Error:=eeInvalidType;
            end;
          end;
          else Error:=eeInvalidType;
        end;
      end;
      SkipSpaces;
    end;
  end;

  procedure BitwiseAnd;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    Equals;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and ((Code[Head]='&') and (not InvalidPunctuation(Code[head + 1]))) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      Inc(Head);
      case XType of
        eeFloat: begin
          Equals;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              IVal:=Trunc(OFVal) and Trunc(FVal);
              XType:=eeInt;
            end;
            eeInt: IVal:=Trunc(OFVal) and IVal;
            else Error:=eeInvalidType;
          end;
        end;
        eeInt: begin
          Equals;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              IVal:=OIVal and Trunc(FVal);
              XType:=eeInt;
            end;
            eeInt: IVal:=OIVal and IVal;
            else Error:=eeInvalidType;
          end;
        end;
        else Error:=eeInvalidType;
      end;
      SkipSpaces;
    end;
  end;

  procedure BitwiseOr;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    BitwiseAnd;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and ((Code[Head]='|') and (not InvalidPunctuation(Code[head + 1]))) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      Inc(Head);
      case XType of
        eeFloat: begin
          BitwiseAnd;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              IVal:=Trunc(OFVal) or Trunc(FVal);
              XType:=eeInt;
            end;
            eeInt: IVal:=Trunc(OFVal) or IVal;
            else Error:=eeInvalidType;
          end;
        end;
        eeInt: begin
          BitwiseAnd;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              IVal:=OIVal or Trunc(FVal);
              XType:=eeInt;
            end;
            eeInt: IVal:=OIVal or IVal;
            else Error:=eeInvalidType;
          end;
        end;
        else Error:=eeInvalidType;
      end;
      SkipSpaces;
    end;
  end;

  procedure LogicalAnd;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    BitwiseOr;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and ((Code[Head]='&') and (Code[Head + 1]='&')) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      Inc(Head, 2);
      case XType of
        eeFloat: begin
          BitwiseOr;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              if (OFVal <> 0) and (FVal <> 0) then IVal:=1 else IVal:=0;
              XType:=eeInt;
            end;
            eeInt: if (OFVal <> 0) and (IVal <> 0) then IVal:=1 else IVal:=0;
            else Error:=eeInvalidType;
          end;
        end;
        eeInt: begin
          BitwiseOr;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              if (OIVal <> 0) and (FVal <> 0) then IVal:=1 else IVal:=0;
              XType:=eeInt;
            end;
            eeInt: if (OIVal <> 0) and (IVal <> 0) then IVal:=1 else IVal:=0;
            else Error:=eeInvalidType;
          end;
        end;
        else Error:=eeInvalidType;
      end;
      SkipSpaces;
    end;
  end;

  procedure LogicalOr;
  var
    OFVal: Extended;
    OIVal: Int64;
  begin
    LogicalAnd;
    if Error <> eeNoError then exit;
    SkipSpaces;
    while (Head < Len) and (Error=eeNoError) and ((Code[Head]='|') and (Code[Head + 1]='|')) do begin
      OFVal:=FVal;
      OIVal:=IVal;
      Inc(Head, 2);
      case XType of
        eeFloat: begin
          LogicalAnd;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              if (OFVal <> 0) or (FVal <> 0) then IVal:=1 else IVal:=0;
              XType:=eeInt;
            end;
            eeInt: if (OFVal <> 0) or (IVal <> 0) then IVal:=1 else IVal:=0;
            else Error:=eeInvalidType;
          end;
        end;
        eeInt: begin
          LogicalAnd;
          if Error <> eeNoError then exit;
          case XType of
            eeFloat: begin
              if (OIVal <> 0) or (FVal <> 0) then IVal:=1 else IVal:=0;
              XType:=eeInt;
            end;
            eeInt: if (OIVal <> 0) or (IVal <> 0) then IVal:=1 else IVal:=0;
            else Error:=eeInvalidType;
          end;
        end;
        else Error:=eeInvalidType;
      end;
      SkipSpaces;
    end;
  end;

  procedure SubExpression; inline;
  begin
    LogicalOr;
    if Error=eeInvalidExpression then begin
      Error:=eeNoError;
      IVal:=1;
      XType:=eeInt;
    end;
  end;


begin
  Code:=TLIL.ToString(AValue);
  Len:=AValue.Length;
  Head:=1;
  IVal:=0;
  FVal:=0;
  XType:=eeInt;
  Error:=eeNoError;

  SubExpression;

  if Error <> eeNoError then begin
    case Error of
      eeSyntaxError: LIL.SetError('expression syntax error');
      eeInvalidType: LIL.SetError('mixing invalid types in expression');
      eeDivisionByZero: LIL.SetError('division by zero in expression');
    end;
    exit(nil);
  end;
  if XType=eeInt then Result:=TLIL.AllocInteger(IVal) else Result:=TLIL.AllocFloat(FVal);
end;

function TLIL.EvaluateExpressionValue(AValue: TLILValue): TLILValue;
begin
  AValue:=SubstituteToValue(AValue);
  if AValue.Equals('') then begin
    FreeAndNil(AValue);
    exit(TLIL.AllocInteger(0));
  end;
  Result:=LIL_EvaluateExpressionValue(self, AValue);
  FreeAndNil(AValue);
end;

function TLIL.Parse(ACommand: string; FuncLevel: Boolean=False): TLILValue;
label cleanup;
var
  SaveCode: string;
  SHead, SaveHead, SaveCLen, i: Integer;
  SaveFixHead: Boolean;
  Words: TLILList;
  Func: TLILFunction;
  Args: TLILValue;
begin
  SaveCode:=Code;
  SaveHead:=Head;
  SaveCLen:=CLen;
  Code:=ACommand;
  CLen:=Length(Code);
  Inc(ParseDepth);
  if ParseDepth=1 then begin
    RootCode:=Code;
    Env:=RootEnv;
    FError:=False;
    FErrorMessage:='';
    FErrorHead:=1;
  end;
  Head:=1;
  SkipSpaces;
  Words:=nil;
  Result:=nil;
  if FuncLevel then Env.BreakRun:=False;
  while (Head <= CLen) and (not FError) do begin
    if Words <> nil then FreeAndNil(Words);
    if Result <> nil then FreeAndNil(Result);
    Words:=Substitute;
    if (Words=nil) or FError then goto cleanup;
    if Words.Count > 0 then begin
      Func:=FindFunction(ToString(Words[0]));
      if Func=nil then begin
        if Words[0].Length > 0 then begin
          if Catcher <> '' then begin
            if InCatcher < MAX_CATCHER_DEPTH then begin
              Inc(InCatcher);
              PushEnv;
              Env.CatcherFor:=Words[0].Clone;
              Args:=Words.ToValue;
              SetVar('args', Args, lsvlLocalNew);
              FreeAndNil(Args);
              Result:=Parse(Catcher, True);
              PopEnv;
              Env.CatcherFor.Free;
              Env.CatcherFor:=nil;
              Dec(InCatcher);
            end else begin
              SetError('Catcher limit reached while trying to call unknown function ' + ToString(Words[0]));
              goto cleanup;
            end;
          end else begin
            SetError('Unknown function ' + ToString(Words[0]));
            goto cleanup;
          end;
        end;
      end else begin
        if Func.Native then begin
          SHead:=Head;
          SaveFixHead:=FixHead;
          FFuncName:=ToString(Words[0]);
          Result:=Func.Proc(Self, Words.ToFunctionArgs);
          if FixHead then Head:=SHead;
          FixHead:=SaveFixHead;
        end else begin
          PushEnv;
          Env.Func:=Func;
          if (Func.ArgNames.Count=1) and (ToString(Func.ArgNames[0])='args') then begin
            Args:=Words.ToValue;
            SetVar('args', Args, lsvlLocalNew);
            FreeAndNil(Args);
          end else begin
            for i:=0 to Func.ArgNames.Count - 1 do begin
              if i < Words.Count - 1 then
                SetVar(ToString(Func.ArgNames[i]), Words[i + 1], lsvlLocalNew)
              else
                SetVar(ToString(Func.ArgNames[i]), Empty, lsvlLocalNew);
            end;
          end;
          Result:=ParseValue(Func.Code, True);
          PopEnv;
        end;
      end;
    end;
    if Env.BreakRun then goto cleanup;
    SkipSpaces;
    while AtEol do Inc(Head);
    SkipSpaces;
  end;

cleanup:
  if FError and Assigned(FOnError) then FOnError(Self, ErrorHead, ErrorMessage);
  FreeAndNil(Words);
  Head:=SaveHead;
  CLen:=SaveCLen;
  Code:=SaveCode;
  if FuncLevel and Env.RetValSet then begin
    FreeAndNil(Result);
    Result:=Env.RetVal;
    Env.RetVal:=nil;
    Env.RetValSet:=False;
    Env.BreakRun:=False;
  end;
  Dec(ParseDepth);
  if Result=nil then Result:=TLILValue.Create;
end;

function TLIL.ParseValue(ACmdValue: TLILValue; FuncLevel: Boolean=False): TLILValue;
begin
  if (ACmdValue=nil) or (ACmdValue.Length=0) then exit(TLILValue.Create);
  Result:=Parse(ToString(ACmdValue), FuncLevel);
end;

function TLIL.ToString: string;
begin
  Result:='<LIL Runtime>';
end;

{$HINTS+}

initialization
  LILFormatSettings:=DefaultFormatSettings;
  with LILFormatSettings do begin
    ThousandSeparator:=',';
    DecimalSeparator:='.';
  end;
  {$IFDEF LAZFPLIL}
  {$I fplilpackage.lrs}
  {$ENDIF}
end.

