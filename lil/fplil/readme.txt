FPLIL: A Little Interpreted Language (Free Pascal port)
=======================================================

0. Contents
-----------
  1. About
  2. Free Pascal API
     2.1. TLILValue
     2.2. TLILVariable
     2.3. TLILEnvironment
     2.4. TLILList
     2.5. TLILFunction
     2.6. TLIL
  3. Integrating FPLIL in Free Pascal programs
  4. Contact


1. About
--------
   LIL (Little Interpreted Language) is a small C library in just a pair of
 .c and .h files which provide a compact yet very dynamic scripting language
 inspired by Tcl and the UNIX shell.  LIL can be used in a wide range of
 situations, from simple configuration files to full extendability via
 scripting.

   FPLIL is a Free Pascal port of LIL in the OBJFPC compiler mode.  The source
 code consists of a single .pas file (the fplil unit) which is written against
 the latest version of Free Pascal at the moment of the port (2.4.2).  No
 other Free Pascal versions or Pascal compilers have been tested.  However the
 code might compile under Delphi with some modifications.

   This readme file does not describe the LIL syntax or the C API (which is
 also used by the Free Pascal bindings to the C library with the unit provided
 by the Import LIL project).  You need to read the C LIL's readme.txt file
 for that.  This readme file describes the Free Pascal API which uses a
 slightly more object-oriented approach. 
 
   Note that any difference between the C LIL and the Free Pascal LIL should
 be considered as a bug in the Free Pascal LIL.  The C LIL is the "canonical"
 version of LIL.


2. Free Pascal API
------------------
   The Free Pascal version of LIL uses a more FreePascal-ish object oriented API
 than the pure procedural C version.  The API is accessed mainly via the TLIL
 class, which represents a LIL runtime and the TLILValue, TLILVariable,
 TLILEnvironment and TLILFunction classes.  The classes that a FPLIL user will
 concern himself most commonly are TLIL and TLILValue.  The public interface
 for each class is documented below.

2.1. TLILValue
     ---------
   This class represents a value in the LIL runtime.  Internally the value is
 stored as a string (at least in the current implementation), but can be
 accessed as a string, integer, floating point number or boolean.  The public
 functions and properties are:

      procedure AppendChar(Ch: Char);
      Appends the character Ch to the end of the value

      procedure AppendString(Str: string);
      Appends the string Str to the end of the value

      procedure AppendValue(AValue: TLILValue);
      Appends the value AValue to the end of the value

      function Clone: TLILValue;
      Creates a clone of this value

      function Equals(StrVal: string): Boolean;
      Returns True if the value equals to the string StrVal

      function Equals(IntVal: Int64): Boolean;
      Returns True if the value equals to the integer IntVal

      function Equals(FloatVal: Extended): Boolean;
      Returns True if the value equals to the floating point number FloatVal

      function Equals(LILVal: TLILValue): Boolean;
      Returns True if the value equals to the LIL value LILVal

      property StringValue: string (read only)
      Returns the value as a string

      property IntegerValue: Int64 (read only)
      Returns the value as an integer

      property FloatValue: Extended (read only)
      Returns the value as a floating point number

      property BooleanValue: Boolean (read only)
      Returns the value as a boolean value (using LIL's boolean logic)

      property Length: Integer (read only)
      Returns the length of the value in characters (assuming 8bits per char)

2.2. TLILVariable
     ------------
   This class represents a variable in LIL.  The public functions and
 properties are:

      property Name: string (read only)
      Returns the variable's name

      property Environment: TLILEnvironment (read only)
      Returns the variable's enviroment

      property Value: TLILValue (read write)
      Returns or sets the variable's value.  Note that setting this property
      will actually create a clone of the value so it is safe to use temporary
      values here

   Note that it is recommended to use TLIL's SetVar and GetVar methods to
 set/modify/access variables instead of using TLILVariable instances directly.

2.3. TLILEnvironment
     ---------------
   This class represents an execution environment (which mostly contains a
 list with the variables in the current scope) and should be treated as an
 opaque object.

2.4. TLILList
     --------
   This class is used to hold a list of LIL values and to encode such lists to
 LIL values (a TLILValue cannot hold a TLILList and must be encoded to one to
 use it).  The public functions and properties are:

      procedure Add(AValue: TLILValue);
      Add the LIL value AValue to the end of the list

      procedure AddString(AString: string);
      Add the string AString to the end of the list

      procedure AddFloat(AFloat: Extended);
      Add the floating point number AFloat to the end of the list

      procedure AddBoolean(ABoolean: Boolean);
      Add the boolean ABoolean to the end of the list

      function IndexOf(AValue: TLILValue): Integer;
      Returns the index of a value in the list that equals (using the Equals
      method) to the given value or -1 if the value was not found

      function IndexOfString(AString: string): Integer;
      Returns the index of a value in the list that equals to the given string
      value or -1 if the value was not found

      function IndexOfInteger(AInteger: Int64): Integer;
      Returns the index of a value in the list that equals to the given integer
      value or -1 if the value was not found

      function IndexOfFloat(AFloat: Extended): Integer;
      Returns the index of a value in the list that equals to the given
      floating point number or -1 if the value was not found

      function IndexOfBoolean(ABoolean: Boolean): Integer;
      Returns the index of a value in the list that equals to the given boolean
      value or -1 if the value was not found

      function Has(AValue: TLILValue): Boolean;
      Returns True if any of the values in the list is equal (using the Equals
      method) to the given value

      function HasString(AString: string): Boolean;
      Returns True if any of the values in the list is equal to the given
      string value

      function HasInteger(AInteger: Int64): Boolean;
      Returns True if any of the values in the list is equal to the given
      integer value

      function HasFloat(AFloat: Extended): Boolean;
      Returns True if any of the values in the list is equal to the given
      floating point number value

      function HasBoolean(ABoolean: Boolean): Boolean;
      Returns True if any of the values in the list is equal to the given
      boolean value

      function ToValue(DoEscape: Boolean=True): TLILValue;
      Encode the list to a LIL value.  If DoEscape is True (the default),
      proper escaping will be added for LIL symbols

      property Count: Integer (read only)
      Returns the number of values in the list

      property Values[AIndex: Integer]: TLILValue (read only, default)
      Returns the value at AIndex

   To create a list from a LIL value you can use TLIL's SubstituteToList
 method.

2.5. TLILFunction
     ------------
   This class represents a LIL function (both native functions and functions
 defined by the script).  The public functions and properties are:

      property Name: string (read only)
      Returns the function's name

      property Body: string (read only)
      Returns the function's body (the code, if a script defined function)

      property Native: Boolean (read only)
      Returns True if the function is a native function or False if it is a
      script defined function

      property FunctionProc: TLILFunctionProc (read only)
      Returns the function procedure if the function is a native function

      property Arguments: Integer (read only)
      Returns the number of arguments this function accepts (for script
      defined functions only)

      property Argument[AIndex: Integer]: string (ready only)
      Returns the argument name at AIndex (for script defined functions only)

2.6. TLIL
     ----
   This class is the most important class for FPLIL and the "entry" to using
 LIL in a Free Pascal program.  It represent a LIL runtime and it is a
 TComponent descendant so it can be used as a FCL/LCL component.  The public
 functions and properties are:

      class function AllocString(Str: string): TLILValue;
      Allocate a string value

      class function AllocInteger(Int: Int64): TLILValue;
      Allocate an integer value

      class function AllocFloat(Float: Extended): TLILValue;
      Allocate a floating point number value

      class function AllocBoolean(Bool: Boolean): TLILValue;
      Allocate a boolean value

      class function ToString(AValue: TLILValue): string;
      Returns the value as a string or an empty string if the value is nil

      class function ToInteger(AValue: TLILValue): Int64;
      Returns the value as an integer or 0 if the value is nil

      class function ToFloat(AValue: TLILValue): Extended;
      Returns the value as a floating point number or 0 if the value is nil

      class function ToBoolean(AValue: TLILValue): Boolean;
      Returns the value as a boolean or False if the value is nil

      procedure SetError(AErrorMessage: string);
      Set an error message (it will be captured by 'try' later, if it is set)

      procedure Register(AName: string; AProc: TLILFunctionProc);
      Register a native LIL function with the given name and function
      procedure.  The related TLILFunctionProc and TLILFunctionProcTypes are
      defined as:
        TLILFunctionProcArgs = array of TLILValue;
        TLILFunctionProc = function(LIL: TLIL; Args: TLILFunctionProcArgs):
                           TLILValue;
      The native function will be called with the passed arguments in Args and
      will have to return a LIL value (allocated with one of the TLIL.Alloc???
      functions above) or nil if the function is not to return a value (or
      returns an empty/False value)

      function GetFunction(AName: string): TLILFunction;
      Returns the function object for the function with the given name or nil
      if no such function exists

      function SetVar(AName: string; AValue: TLILValue; Locality:
                      Locality: TLILSetVarLocality): TLILVariable;
      Sets the given variable to the given value.  The Locality parameter is
      used to decide the scope of the variable and can be one of:
        lsvlGlobal   - If the variable does not exist, create a global one
        lsvlLocal    - If the variable does not exist, create a local one
        lsvlLocalNew - Always create a new local variable even if it exists
      The function returns the variable's object

      function GetVar(AName: string; DefaultValue: TLILValue=nil): TLILValue;
      Returns the value of the given variable at the current scope or the given
      default value if no variable with the given name is found

      function UnusedName(Part: string): string;
      Returns an unused name (both in functions and variables).  The Part
      parameter is used as part of the name

      function PushEnv;
      Push the current environment down the environment stack and create a new
      one at the top.  Use this before calling LIL code from a LIL native
      function to cause any local variable defined in the code be lost in the
      next call to PopEnv

      function PopEnv;
      Pop the environment from the environment stack and delete it.  PushEnv
      and PopEnv must always match each other

      function SubstituteToList(ACode: TLILValue): TLILList;
      Perform a single substitution pass on the given code and return the words
      that make up the code in the given LIL list.  See the C LIL documentation
      for how substitution is done

      function SubstituteToValue(ACode: TLILValue): TLILValue;
      Like SubstituteToList but encodes the list to a value

      function EvaluateExpression(ACode: string): TLILValue;
      Evaluate the expression in the given string like the expr function does

      function EvaluateExpressionValue(AValue: TLILValue): TLILValue;
      Evaluate the epxression in the given value like the expr function does

      function Parse(ACommand: string; FuncLevel: Boolean=False): TLILValue;
      Parse and execute the LIL code in ACommand and return it's result.  The
      FuncLevel determines what the "result" is supposed to be: if True, the
      result is set using the return function (like in a script defined
      function), otherwise it is the result of the last command.  The value
      must be freed (if not nil)

      function ParseValue(ACmdValue: TLILValue; FuncLevel: Boolean=False):
               TLILValue;
      Like Parse, but uses a lil value for the command

      function ToString: string;
      Simply returns '<LIL Runtime>'

      property FuncName: string (read only)
      Returns the currently executed native function name.  This should be
      used only from within native function procedures.

      property Error: Boolean (read only)
      Returns True if there was an error in the last call to Parse/ParseValue

      property ErrorHead: Integer (read only)
      Returns the character where the error occured.  The character is in the
      string passed to Parse/ParseValue

      property ErrorMessage: string (read only)
      Returns the error message

      property Data: Pointer (read write)
      Custom data pointer

   The class also defines the following events:

      OnExit(LIL: TLIL; Arg: TLILValue)
      Called when the exit function is called.  If this is not set, the Halt
      function will be called.  The Arg parameter is the first argument passed
      to the exit function

      OnWrite(LIL: TLIL; Chars: string)
      Called when the write or print function is called.  The #10 character is
      used for newline by print.  If this is not set, the Write/WriteLn
      functions are used

      OnRead(LIL: TLIL; AFileName: string; out AContent: string)
      Called when the read or source (when the OnSource is not set) function
      is called to read the text from the given file in the AContent parameter
      (it is expected that if the read fails, the AContent will be set to an
      empty string).  If this is not set, the Assign/Reset/Close functions are
      used

      OnStore(LIL: TLIL; AFileName, AContent: string)
      Called when the store function is called to store the given content to
      the given filename.  If not set, the Assign/Rewrite/Close functions are
      used

      OnSource(LIL: TLIL; AFileName: string; out AContent: string)
      Like OnRead, but called only when the source function is called.  If not
      set, the OnRead handler is called and if that isn't set either, then the
      Assign/Reset/Close functions are used

      OnError(LIL: TLIL; APosition: Integer; AMessage: string)
      Called when an error occurs at the passed position with the passed
      message

      OnSetVar(LIL: TLIL; AName: string; const AValue: TLILValue;
               var AReplacement: TLILValue;
               var ASetVarAction: TLILOnSetVarAction)
      Called each time before a variable is set for the host program to
      inspect the variable's name and value and decide what to do with it using
      the ASetVarAction parameter.  The parameter can take one of the following
      values:
        losaDefault - default action (store the value to the variable normally)
        losaReplace - store the AReplacement value to the variable
        losaIgnore  - do nothing (if the variable isn't set it won't be made)

      OnGetVar(LIL: TLIL; AName: string; const AValue: TLILValue;
               var AReplacement: TLILValue;
               var AGetVarAction: TLILOnGetVarAction)
      Called each time a variable is about to be read for the host program to
      inspect the variable's name and value and decide what to do with it using
      the AGetVarAction parameter.  The parameter can take one of the following
      values:
        logaDefault - default action (return the variable's value)
        logaReplace - return the AReplacement value
      

3. Integrating FPLIL in Free Pascal programs
--------------------------------------------
   To use FPLIL simply create a TLIL instance like

      var LIL: TLIL;

      LIL:=TLIL.Create(nil);
   
   To register a custom function, use LIL.Register, like

      function FncMul(LIL: TLIL; Args: TLILFunctionProcArgs): TLILValue;
      begin
        if Length(Args) < 2 then exit(nil);
        Result:=TLIL.AllocFloat(TLIL.ToFloat(Args[0])*TLIL.ToFloat(Args[1]));
      end;

      LIL.Register('mul', @FncMul);

   To execute some LIL code, use LIL.Parse, like

      LIL.Parse('print [mul 3 4]').Free;

   See the example.pas for a complete example with the above.


4. Contact
----------
  Kostas Michalopoulos
  badsector@runtimelegend.com
  badsectoracula@gmail.com

  also see http://runtimelegend.com/rep/lil
