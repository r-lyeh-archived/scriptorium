LIL: A Little Interpreted Language
==================================


0. Contents
-----------
  1. About
  2. LIL syntax
  3. LIL functions
     3.1. Standard LIL functions
     3.2. Command line interpreter LIL functions
  4. Integrating LIL in C programs
     4.1. Initialize LIL
     4.2. Execute LIL code
     4.3. Raise an error and obtain information about errors
     4.4. Convert values between C and LIL
     4.5. Register native functions
     4.6. Other LIL library functions
     4.7. LIL callback summary
     4.8. Using LIL as a DLL
  5. Integrating LIL in non-C programs
  6. Contact


1. About
--------
   LIL (Little Interpreted Language) is a small C library in just a pair of
 .c and .h files which provide a compact yet very dynamic scripting
 language inspired by Tcl and the UNIX shell.  LIL can be used in a wide
 range of situations, from simple configuration files to full extendability
 via scripting.
 
   The source code of LIL consists of a pair of .c and .h files (lil.c and
 lil.h) of ANSI C90 with some extensions from C99 (mostly the use of 64bit
 integers) which most modern compilers provide. The code has been tested to
 compile and work with the following compilers:
 
     * GNU C/C++ Compiler 3.x and 4.x
     * OpenWatcom C/C++ Compiler 1.9
     * LLVM Clang
     * Digital Mars C/C++ Compiler 8.42n
     * Tiny C Compiler 0.9.25
     * Microsoft Visual C++ 2010 Express (see below for older versions)
     * Microsoft Visual C++ 2012 Express
     * Borland's free C/C++ Compiler 5.5.1 (needs special Makefile)

 It has been tested under the following platforms:

     * Windows x86, x86_64 (Desktop Mode)
     * Linux x86, x86_64
     * Mac OS X (32bit and 64bit)
     * iOS
     * Android
 
 Users of older versions of Microsoft Visual C++ need to use a stdint.h
 file provided by external sources.  A commonly used one stdint.h file for
 MSVC is http://msinttypes.googlecode.com/svn/trunk/stdint.h.  Alternatively
 the global macro LILINT_INT64 can be used so that LIL will use __int64 for
 integers instead of stdint.h's int64_t.

 If the compiler doesn't use long long int nor __int64 for 64bit integers,
 then the global macro LILINT_CUSTOM must be defined and a type definition
 for lilint_t must exist before the inclusion of lil.h (including inside the
 lil.c file).
 
 As a side note, LIL has nothing to do with the "Little Implementation
 Language" for PDP (which i learned about months after i chose the name
 LIL for my library).  Apparently that LIL was made during the same time as
 the UNIX system and the C language by P.J.  Plauger.  If you are interested
 in this historical language you can read about it here:
 
     http://www.ultimate.com/phil/lil/


2. LIL syntax
-------------
   The syntax of LIL is very simple: the script is made up of "commands"
 and each command is a series of "words" separated by space, like:
 
     word1 word2 word3 ... wordn
 
   Words can be enclosed in quotes, double quotes and brackets which allow
 for special handling.  Quotes and double quotes, for example, can be used
 to include spaces or special characters (by escaping them with the \
 prefix as in C).  Square brackets are used to call inline commands and
 substitute the inlined command with its result.  Curly brackets are used
 to refer to a string without further processing (except counting the
 brackets inside them so that they can be nested).  A dollar in front of
 a word will replace itself and the word with the value of the variable
 that has the same name as the word (or with nothing if there is no such
 variable).
 
   When LIL needs to execute a command, it looks for a function with the
 same name as the first word of the command.  The function is executed
 using the rest of the words as its arguments (or parameters).  For example
 the following command
 
     print hello fred
 
 will execute the function "print" using the two words "hello" and "fred"
 as the first and second argument.  Using quotes one can combine the two
 words like
 
     print "hello fred"
 
 in which case LIL will execute the function "print" with the single word
 "hello fred" (note that a "word" in LIL can have any character, including
 spaces and even special characters by escaping them).  Since the words are
 separated before LIL decides which is function and which not, technically
 you can use functions with spaces, like
 
     "hello fred" print
     
 which will execute the function "hello fred" with the single word "print"
 as its argument.  In practice this should be avoided, at least for names
 that the script writer will use directly.
 
   As mentioned above the quotes can be used to escape characters, so you
 can write
 
     print "hello \"fred\""
   
   An alternative would be to use the single quotes (to avoid the need of
 using the escape character for double quotes) like
 
     print 'hello "fred"'
   
 or use curly braces like
 
     print {hello "fred"}
     
   Curly braces aren't always an alternative to single and double quotes:
 unlike them, curly braces do not do any further processing (except to
 count the curly braces inside so they can know where they end).  So this
 command
 
     print {hello \"fred\"}
 
 will include the \ character while this command
 
     print 'hello \"fred\"'
 
 will not because \" will be converted to ".
 
   Quotted strings can contain escape characters. These are characters
 which you either can't use or special characters you can't (or don't want)
 to type in the code.  As shown above, an escaped character starts with
 the \ character (the "escape character") and followed by another character
 which defines which character will be inserted at that point.  The escape
 characters LIL recognizes as special characters are
 
     b - inserts the backspace character
     t - inserts the horizontal tab character
     n - inserts the newline character
     v - inserts the vertical tab character
     f - inserts the formfeed character
     r - inserts the carriage return character
     a - inserts the bell character
 
 any other character will be included as-is without the \ character.  For
 example the string "hello\nworld" will insert a newline character between
 "hello" and "world".  The string "is this good\?" will be equal to the
 string "is this good?" and the string "hello \[world\]" will be equal
 to "hello [world]" (however LIL will not try to evaluate "world" as a
 command - see below).
 
   As mentioned above, square braces are used to replace (substitute) the
 command inside them with the result of that command.  In LIL this is used
 to access the result of a function.  As an example consider mathematical
 expressions: LIL does not have special expression handling.  To use math
 you have to type the expression as one or more arguments to the "expr"
 function which will combine all arguments, parse the expression as a
 single string and return the result.  So the command
 
     expr 4 + 4
 
 will have the result "8".  This result will not be shown anywhere - it is
 just the result of the command.  To access this result you use the square
 brackets with some other command.  For example to display the command with
 the print function use the expression inside the brackets like
 
     print [expr 4 + 4]
 
 this will print "8".  Here LIL actually sees two words, not five: the first
 word is the "print" word and the second word is the result of the command
 inside the brackets: when LIL sees the brackets, it scans its contents for
 an inline command.  Then it executes that command (which in turn might
 contain other inline commands) and considers the word as the executed
 command's result instead of the brackets' contents.  Note that if you
 typed
 
     print expr 4 + 4
 
 the word would be "print", "expr", "4", "+", "4" and the print function
 would simply display "expr 4 + 4" because without the square brackets
 there is not any special meaning for "expr".

   A very important function for LIL (beyond expr) is the "set" function.
 This function can be used to set values to variables.  For example when
 you execute
 
     set foo bar
 
 you set the value of the "foo" variable to the string "bar".  As you might
 have guessed if type
 
     set "foo bar" "moo mew"
 
 you will set the value of the "foo bar" variable to the string "moo mew":
 LIL can use any valid character as a name, as long as you can express it
 as a string.  The set function can be used with zero or more arguments.
 For each pair, a variable named as the first word of the pair will be set
 to the second word of the pair.  If there is an odd number of arguments,
 the result of the set function is the value of the variable that has the
 same name as the last word.  So for example the command
 
     set name

 will have a result equal with the value of the variable "name" and thus
 the command
 
     print [set name]
 
 will print the value of the variable "name".  Since accessing variables is
 something very common LIL provides a shortcut: a dollar in front of a word
 is the same as calling set with that word as a single argument.  So the
 command above could be written as
 
     print $name
 
 Note two important things however: first, the name of the variable is NOT
 "$name".  It is "name".  The dollar is used as a shortcut for "[set name]".
 And second, it *really* is a shortcut: the "set" function will be called
 even if you don't type it (so if you override it, the overridden function
 will be called).  You can also change the behavior of $ by setting another
 function to be called with "reflect dollar-prefix" (see section 3 for
 details).
 
   Remember the part about curly brackets above? Well, if you type
   
     print "hello $user"
   
 the result will be to display "hello " followed with whatever the value of
 the variable "user" is (so for example if "user" was set to "fred" the
 displayed string would be "hello fred").  However if you type
 
     print {hello $user}
 
 the displayed string will be "hello $user" because curly brackets really
 do not process whatever is inside.  This can be used to define new
 functions because the function's code is, like anything else in LIL, a
 plain old string.  And since you don't want to process that string's
 contents directly, you need curly brackets.

   The definition of a new function is done using the "func" function.  This
 function accepts zero or more arguments (when zero arguments are given,
 the function does nothing and returns an empty string).  When three
 arguments are given (the most common case) they are considered as the name
 the argument list and the code/body of the function.  For example
 
     func foo {a b} { print $a $b }
 
 defines a function named "foo" with the argument list "a b" and as code
 the string "print $a $b".  The argument list is a LIL list (a space
 separated list of words using similar rules as the separation of words for
 commands - which in turn can include other sublists but escaping their
 words, etc - with the exception that a LIL list value can include newline
 characters) where each word in the list specifies an argument and the code
 is the code that will be executed when the function is evaluated.  Before
 LIL evaluates a function, it stores in local variables with the same name
 as those specified in the argument list the arguments passed to the
 function in the same order as they appear in the argument list.  So for
 example when evaluating the above function LIL will save the first
 argument passed to the function in "a" and the second argument in "b".  If
 more arguments are given these are lost and if less arguments are given,
 the rest of the expected arguments are set to empty strings (so for
 example if "foo 32" is given the "b" variable will be an empty string).
 
   To use an unknown number of variables use the special name "args" as the
 only word in the argument list.  LIL will store all passed arguments in
 the "args" argument as a list which you can access either directly or via
 the list functions (index, count, foreach, etc).  For example:
 
     func foo args { foreach $args { print arg: $i } }
 
 which when used like
 
     foo apple orange cake
 
 will display
 
     arg: foo
     arg: apple
     arg: orange
     arg: cake
 
 (when the special "args" name is used, the first word in the list will
 always be the function's name)
 
   The "func" function can also be used with two or even one argument.
 This use allows the definition of anonymous (or actually, randomly named)
 functions.  In the two arguments format, the first argument is the
 arguments list as previously and the second argument is the function's
 code.  For example
 
     func {a b} { return [expr $a + $b] }
 
 returns an anonymous function which accepts two arguments and returns
 their sum.  In the one argument format, the only argument is the
 function's code and the arguments are stored in the "args" variable as if
 you used the special "args" name.  For example
 
     func { foreach $args { print $i is good } }
 
 returns an anonymous function which prints "... is good" for each one of
 the arguments LIL passes to it.
 
   So how are anonymous functions used? Remember the basic syntax of LIL
   
     word1 word2 word3 ... wordn
     
 and that that when LIL wants to execute the command the first word is the
 name of the function? Well, word1 is no different than word2, word3, etc
 and can be a variable reference too.  So if you type
 
     set foo print
     $foo hello
 
 LIL will replace "$foo" with "print" as normal and then will execute the
 function "print" because that happens to be the first word.  So you use
 anonymous functions in the same way: anonymous functions aren't really that
 anonymous but they have a generated name.  That name is what "func"
 returns.  So if you type
 
     set foo [func {a b} { return [expr $a + $b] }]
     print [$foo 3 2]
 
 LIL will define a randomly named function with the argument list "a b" and
 code "return [expr $a + $b]", set its random name to the "foo" variable
 and - in the next line - call it with the arguments 3 and 2 and pass the
 result to print which in turn will display it - "5".

   LIL itself does not require the use of anonymous functions, but they can
 be handy in more advanced scripts and situations.  An example would be a
 function which returns another function which is composed using the
 arguments to the first function.

   When using functions you should keep in mind variable scope: variables
 are defined in global scope (visible and accessible from everywhere) and
 local scope.  Local scope has variables defined from within the function
 itself.  When you access a variable for reading it (like when using it
 with the dollar) LIL looks in the local environment (where the variables
 are stored) and then the global environment.  When you use the "set"
 function to set the value of a variable LIL always look first in the
 local environment and if the variable isn't found, it looks in the global
 environment.  If no variable is found, a new variable in the local
 environment will be created.  To force a global variable to be set or
 created, put the special "global" word as the first argument to the set
 function, like
 
     set global level 32
 
 which sets the value of the global variable "level" to "32".  Functions
 that create variables always provide this special "global" first argument
 to force the assignment of global variables.  Note that the use of
 "global" is only needed for code that runs from functions.  Code that is
 executed outside a function always uses the global environment.  To
 force a local variable, you need to create an empty local variable first
 by calling the "local" function, like

    local level
    set level 32

 which first creates a local variable (even if a global with the same name
 exists) with an empty value and then it sets its value to 32 (since the
 set function first looks in the local environment, it will find the one
 just made and will ignore any variable that may be in the global one).

   The last note on LIL's syntax is word concatenation.  This is really
 simple (and technically is not concatenation at all but it is easier to
 think it like this): if two words are not separated by space they are
 combined to a single word.  So for example the command
 
     print "hello "world
 
 is made up of two words: "print" and "hello world".  Similarly the command
 
     print "is it "[expr 3 + 2]?
 
 is made up of two words again: "print" and "is it six?".  This happens
 because the "is it ", [expr 3 + 2] and ? parts are combined together and
 not separated by a space.
 
   LIL commands are separated by newlines (each command takes a whole line)
 or the ; character.  For example, for LIL the code
 
     print hello ; print world
 
 is the same as
 
     print hello
     print world
     
   Finally you can put comments in the code using the # character.  Any
 characters after # until the end of the line will be ignored by the LIL
 interpreter.  To add multiline comments (comments that span multiple
 lines) use ## to begin the comment block and ## again to end it.  For
 example

     print hello
     ## print how are you
        print i feel like i'm
        print going to take over the ##
     print world
 
 In the above example only the "hello" and "world" will be printed.  Note
 that multiline comments apply only when two #s are following by a non-#
 character - for example the following comment is single-line:

     ### This is important!
     printf fail

   For more information and details on LIL's syntax, check the .lil example
 files that come with the source code distribution.  And of course the C
 code itself is clean and compact which makes it easy to read and, if
 needed, modify.


3. LIL functions
----------------

3.1. Standard LIL functions
     ----------------------

   Here is a summary of the functions that LIL provides by default to all
 LIL scripts (in order of appearance in the lil.c file):
 
     reflect
       reflect information about the LIL runtime and the program
 
     reflect version
       returns the LIL_VERSION_STRING
 
     reflect args <func>
       returns a list with the argument names for the given function
     
     reflect body <func>
       returns the code of the given function
     
     reflect func-count
       returns the number of the known functions
     
     reflect funcs
       returns a list with all the known function names
     
     reflect vars
       returns a list with all the known variable names (includes global
       and local variables)
     
     reflect globals
       returns a list with all the known global variable names

     reflect has-func <name>
       returns a true value (non-zero, non-empty value) if a function with
       the given name is known
     
     reflect has-var <name>
       returns a true value (non-zero, non-empty value) if a variable with
       the given name is known
     
     reflect has-global <name>
       returns a true value (non-zero, non-empty value) if a global
       variable with the given name is known
     
     reflect error
       returns the last error message or an empty string if there is no
       error condition active (this is usually used with the try function)

     reflect dollar-prefix [prefix]
       if [prefix] is specified, then this changes the dollar prefix.  If no
       arguments are given, the current dollar prefix is returned.  The dollar
       prefix is the command to be executed for dollar expansions (like $foo).
       The word after the dollar prefix is appended immediately after the
       prefix and the whole is executed.  The default dollar prefix is 'set '
       (notice the space which will separate the call to "set" from the word
       following)

     reflect this
       returns the code of the current local environment.  This will return
       the currently executed function's body, the current root (top-level)
       code or the current catcher code (if the current environment is a
       catcher environment)

     reflect name
       returns the name of the currently executed function or an empty string
       if the code is executed at root level (or the name of the current
       function is unknown)
     
     func [name] [argument list | "args"] <code>
       register a new function.  See the section 2 for more information

     rename <oldname> <newname>
       rename an existing function.  Note that the "set" function is used to
       access variables using the $ prefix so if the "set" function is
       renamed, variables will only be accessible using the new name.  The
       function returns the <oldname>

     unusedname [part]
       return an unused function name.  This is a random name which has the
       form !!un![part]!<some number>!nu!!.  The [part] is optional (if not
       provided "unusedname" will be used)
     
     quote [...]
       return the arguments as a single space-separated string
     
     set ["global"] [name [value] ...]
       set the variable "name" to the "value".  If there is an odd number of
       arguments, the function returns the value of the variable which has
       the same name as the last argument.  Otherwise an empty value is
       returned.  See section 2 for details
     
     local [...]
       make each variable defined in the arguments a local one.  If the
       variable is already defined in the local environment, nothing is done.
       Otherwise a new local variable will be introduced.  This is useful
       for reusable functions that want to make sure they will not modify
       existing global variables

     write [...]
       write the arguments separated by spaces to the program output.  By
       default this is the standard output but a program can override this
       using the LIL_CALLBACK_WRITE callback
     
     print [...]
       like write but adds a newline at the end
     
     eval [...]
       combines the arguments to a single string and evaluates it as LIL
       code.  The function returns the result of the LIL code

     topeval [...]
       combines the arguments to a single string and evaluates it as LIL
       code in the topmost (global) environment.  This can be used to execute
       code outside of any function's environment that affects the global
       one

     upeval [...]
       combines the arguments to a single string and evaluates it as LIL
       code in the environment above the current environment (the parent
       environment).  For functions this is usually the function caller's
       environment.  This can be used to access local variables (for read
       and write purposes) of a function's caller or to affect its flow
       (like causing a caller function to return).  The function can be
       used to provide most of the functionality that other languages
       provide via the use of macros but at the program's runtime and with
       full access to the program's state
     
     downeval [...]
       downeval complements upeval. It works like eval, but the code is
       evaluated in the environment where the most recent call to upeval was
       made.  This also works with topeval

     enveval [invars] [outvars] <code>
       the <code> will be executed in its own environment.  The environment
       will be similar to a function's environment.  If invars is provided,
       it is assumed to be a list with variable names to be copied from the
       current environment to the new environment.  If outvars is provided, it
       is assumed to be a list with variable names to be copied from the new
       environment back to the current one (global variables will remain
       global).  If invars is provided but not outvars, the variables in
       invars will be copied back as if outvars was provided with the same
       variable names.  To make the variables "one way" (that is, to copy
       nothing back) just use an empty list for the outvars argument.  From
       inside enveval both return and an immediate value can be used to
       return a value.  Calling return will not cause the calling function
       to exit

     jaileval ["clean"] <code>
       the <code> will be executed in its own LIL runtime.  Unless "clean"
       is specified, the new LIL runtime will get a copy of the currently
       registered native functions.  The <code> can use "return" to return
       a value (which is returned by jaileval)
     
     count <list>
       returns the number of items in a LIL list
     
     index <list> <index>
       returns the <index>-th item in a LIL list.  The indices begin from
       zero (so 0 is the first index, 1 is the second, etc)

     indexof <list> <value>
       returns the index of the first occurence of <value> in a LIL list.  If
       the <value> does not exist indexof will return an empty string.  The
       indices begin from zero (so 0 is the first index, 1 is the second,
       etc)

     filter [varname] <list> <expression>
       filters the given list by evaluating the given expression for each
       item in the list.  If the expression equals to true (is a non-zero
       number and a non-empty string), then the item passes the filter.
       Otherwise the filtered list will not include the item.  For each
       evaluation, the item's value is stored in the [varname] variable
       (or in the "x" variable if no [varname] was given).  The function
       returns the filtered list

     list [...]
       returns a list with the arguments as its items
     
     append ["global"] <list> <value>
       appends the <value> value to the variable containing the <list>
       list (or creates it if the variable is not defined).  If the "global"
       special word is used, the list variable is assumed to be a global
       variable
     
     slice <list> <from> [to]
       returns a slice of the given list from the index <from> to the index
       [to]-1 (that is, the [to]-th item is not includd).  The indices are
       clamped to be within the 0..<list length> range.  If [to] is not
       given, the slice contains all items from the <from> index up to the
       end of the list

     subst [...]
       perform string substitution to the arguments.  For example the code
       
         set foo bar
         set str {foo$foo}
         print [substr $str]
       
       will print "foobar"
     
     concat [...]
       substitutes each argument as a list, converts it to a string and
       returns all strings combined into one
     
     foreach [name] <list> <code>
       for each item in the <list> list, stores it to a variable named "i"
       and evalues the code in <code>.  If [name] is provided, this will be
       used instead of "i".  The results of all evaluations are stored in a
       list which is returned by the function
     
     return [value]
       stops the execution of a function's code and uses <value> as the
       result of that function (note that normally the result of a function
       is the result of the last command of that function).  The result of
       return is always the passed value

     result [value]
       sets or returns the current result value of a function but unlike
       the return function, it doesn't stop the execution.  If no argument
       is given, the function simply returns the current result value - if
       no previous call to result was made, then this will return an empty
       value even if other calls were made previously.  The result of this
       function when an argument is given, is simply the given argument
       itself
     
     expr [...]
       combines all arguments into a single string and evaluates the
       mathematical expression in that string.  The expression can use the
       following operators (in the order presented):
       
          (a)        - parentheses
       
          -a         - negative sign
          +a         - positive sign
          ~a         - bit inversion
          !a         - logical negation
       
          a * b      - multiplication
          a / b      - floating point division
          a \ b      - integer division
          a % b      - modulo
       
          a + b      - addition
          a - b      - subtraction
       
          a << b     - bit shifting
          a >> b
       
          a <= b     - comparison
          a >= b
          a < b
          a > b
       
          a == b     - equality comparison
            or
          a != b
          
          a & b      - bitwise AND
          
          a || b     - logical OR
          a && b     - logical AND
     
     inc <name> [value]
       numerically add [value] to the variable "name".  If [value] is not
       provided, 1 will be added instead
     
     dec <name> [value]
       numerically subtract [value] to the variable "name".  If [value] is
       not provided, 1 will be subtracted instead
     
     read <name>
       reads and returns the contents of the file <name>.  By default LIL
       will look for the file in the host program's current directory, but
       the program can override this using LIL_CALLBACK_READ.  If the
       function failed to read the file it returns an empty value (note,
       however than a file can also be empty by itself)
     
     store <name> <value>
       stores the <value> value in the file <name>.  By default LIL will
       create the file in the host program's current directory, but the
       program can override ths using LIL_CALLBACK_STORE.  The function will
       always return <value>
     
     if ["not"] <value> <code> [else-code]
       if value <value> evaluates to true (non-zero, non-empty string), LIL
       will evaluate the code in <code>.  Otherwise (and if provided) the
       code in [else-code] will be evaluated.  If the "not" special word is
       used, the check will be reversed.  The function returns the result of
       whichever code is evaluated
     
     while ["not"] <expr> <code>
       as long as <expr> evaluates to a true (or false if "not" is used)
       value, LIL will evaluate <code>.  The function returns the last
       result of the evaluation of <code> or an empty value if no
       evaluation happened (note, however that the last evaluation can
       also return an empty value)
     
     for <init> <expr> <step> <code>
       the loop will begin by evaluating the code in <init> normally.  Then
       as long as the expression <expr> evaluates to a true value, the
       code in <code> will be evaluated followed by the code in <step>.  The
       function returns the result of the last evaluation of <code>
     
     char <code>
       returns the character with the given code as a string.  Note that the
       character 0 cannot be used in the current implementation of LIL since
       it depends on 0-terminated strings.  If 0 is passed, an empty string
       will be returned instead
     
     charat <str> <index>
       returns the character at the given index of the given string.  The index
       begins with 0.  If an invalid index is given, an empty value will be
       returned
     
     codeat <str> <index>
       returns the character code at the given index of the given string.  The
       index begins with 0.  If an invalid index is given, an empty value will
       be returned
     
     substr <str> <start> [length]
       returns the part of the given string beginning from <start> and for
       [length] characters.  If [length] is not given, the function will
       return the string from <start> to the end of the string.  The indices
       will be clamped to be within the string boundaries
     
     strpos <str> <part> [start]
       returns the index of the string <part> in the string <str>.  If
       [start] is provided, the search will begin from the character at
       [start], otherwise it will begin from the first character.  If the
       part is not found, the function will return -1
     
     length [...]
       the function will return the sum of the length of all arguments

     trim <str> [characters]
       removes any of the [characters] from the beginning and ending of a
       string until there are no more such characters.  If the [characters]
       argument is not given, the whitespace characters (space, linefeed,
       newline, carriage return, horizontal tab and vertical tab) are used

     ltrim <str> [characters]
       like "trim" but removes only the characters from the left side of the
       string (the beginning)

     rtrim <str> [characters]
       like "trim" but removes only the characters from the right side of the
       string (the ending)

     strcmp <a> <b>
       compares the string <a> and <b> - if <a> is lesser than <b> a
       negative value will be returned, if <a> is greater a positive an
       if both values are equal zero will be returned (this is just a
       wrap for C's strcmp() function)
     
     streq <a> <b>
       returns a true value if both strings are equal
     
     repstr <str> <from> <to>
       returns the string <str> with all occurences of <from> replaced with
       <to>
     
     split <str> [sep]
       split the given string in substrings using [sep] as a separator and
       return a list with the substrings.  If [sep] is not given, the space
       is used as the separator.  If [sep] contains more than one characters,
       all of them are considered as separators (ie. if ", " is given, the
       string will be splitted in both spaces and commas).  If [sep] is an
       empty string, the <str> is returned unchanged
     
     try <code> [handler]
       evaluates the code in <code> normally and returns its result.  If an
       error occurs while the code in <code> is executed, the execution
       stops and the code in [handler] is evaluated, in which case the
       function returns the result of [handler].  If [handler] is not
       provided the function returns 0
     
     error [msg]
       raises an error.  If [msg] is given the error message is set to <msg>
       otherwise no error message is set.  The error can be captured using
       the try function (see above)
     
     exit [code]
       requests from the host program to exit.  By default LIL will call the
       C function exit() but the host program can override this using the
       LIL_CALLBACK_EXIT callback.  If [code] is given it will be provided
       as a potential exit code (but the program can use another if it
       provides a LIL_CALLBACK_EXIT callback)
     
     source <name>
       read and evaluate LIL source code from the file <name>.  The result
       of the function is the result of the code evaluation.  By default LIL
       will look for a text file in the host program's current directory
       but the program can override that by using LIL_CALLBACK_SOURCE
     
     lmap <list> <name1> [name2 [name3 ...]]
       map the values in the list to variables specified by the rest of the
       arguments.  For example the command
       
         lmap [5 3 6] apple orange pear
         
       will assign 5 to variable apple, 3 to variable orange and 6 to
       variable pear
     
     rand
       returns a random number between 0.0 and 1.0
       
     catcher [code]
       sets, removes or returns the current catcher code.  The code can be
       used to "catch" calls of unknown functions and is executed inside
       its own environment as if an anonymous function without specified
       arguments was called.  This means that the code can access the name
       and the arguments of the function call using the "args" list -
       however unlike anonymous functions which get a random name, the zero
       index of the args list contains the unknown function's name.  The
       code can also use "return" to return some value.  If the catcher code
       calls an unknown function, it will be called again - however to avoid
       infinite loops a limit on the nested calls to the catcher code is set
       using the MAX_CATCHER_DEPTH constant (which by default is set to
       16384).
       
       This function can be used to implement small embedded DSLs in LIL code
       or provide some sort of shell (as in UNIX shell) functionality by
       delegating the unknown function calls to some other function/command
       handler (like executing an external program).
       
       If catcher is called with an empty string, the catcher code is removed
       and LIL will resume raising errors when an unknown function is called
       (which is the default behavior before any call to catcher is made).
       
       If catcher is called without arguments it will return the current
       catcher code.  This can be used to temporary save the current catcher
       code when changing the catcher code temporarily.  For example
       
         set previous-catcher [catcher]
         catcher {print Call failed: $args}
         # do something
         catcher $previous-catcher
         
       When using catcher for DSLs it is recommended to save the previous
       catcher.  For an example of catcher with comments see the catcher.lil
       source file.

3.2. Command line interpreter LIL functions
     --------------------------------------
   The functions shown below are only available to the command line LIL
 interpreter (the "lil" executable).

     writechar code
       writes the character defined by the given code (usually ASCII code)
       to the standard output

     system <args>
       executes the system command defined by <args> and returns the data
       placed in the command's standard output

     readline
       reads a string from the standard input until the end of file or end
       of line mark/character is found and returns it


4. Integrating LIL in C programs
--------------------------------
   To integrate LIL in C programs you have two options:
   
     1. Use the liblil.a library (for Clang and GCC) or liblil.lib (for
        OpenWatcom) as a static library from your project, or
     
     2. Put the lil.c and lil.h files directly in your project
   
   Whichever method you choose, the rest is the same.  Please note that
 the LIL interface should not be considered as frozen at this point.  The
 library is under constant development and might change.  In the future a
 frozen API will be provided but even that will only be guaranteed to
 survive major versions.

   If you need a bit of extra performance, consider compiling lil.c with
 the LIL_ENABLE_POOLS macro defined.  LIL_ENABLE_POOLS will enable value,
 list and environment object pooling which improves memory fragmentation
 and can increase more than 400% the execution speed.  On the other hand,
 pools are global and this makes the library unsafe to use from multiple
 threads even if different threads use different lil_t objects.  Also it
 relies on the runtime/OS to release the memory allocated by the pools
 and increases the total memory needed by LIL.  For these reasons this
 optimization is disabled by default.

4.1. Initialize LIL
     --------------
   You can have several "LILs" running: each one can be separate from the
 others and have its own definitions.  A LIL runtime is stored in the lil_t
 C type that basically specifies an object.  To construct a lil_t object
 use lil_new() and to destroy it use lil_free().  For example:
 
     lil_t lil = lil_new();
     ...
     lil_free(lil);
 
   Before running LIL code you might want to override some of LIL's default
 functionality.  This can be done using the lil_callback() function which
 has the following signature:
 
     void lil_callback(lil_t lil,
                       int cb,
                       lil_callback_proc proc)
     
   The cb is one of the LIL_CALLBACK_* constants defined in the lil.h file
 and the proc is a function with the same signature as the
 lil_callback_*_proc_t type defined in lil.h, casted back to
 lil_callback_proc_t.  For example to define a callback for the "exit"
 function (something you probably want to do in most programs) use the
 following code
 
     void handl_exit(lil_t lil, lil_value_t arg)
     {
         ...
     }
     
     ...

     lil_callback(lil, LIL_CALLBACK_EXIT, (lil_callback_proc_t)handl_exit);
 
4.2. Execute LIL code
     ----------------
   To execute LIL code use the lil_parse() function.  This function has the
 following signature:
 
     lil_value_t lil_parse(lil_t lil,
                           const char* code,
                           size_t codelen,
                           int funclevel)

   If 0 is given for codelen, LIL will simply use strlen for it.  Funclevel
 should always be 1, unless the evaluated code is supposed to be executed
 inside the current environment.  Setting funclevel to 1 will reset (clear)
 the breakrun flag (which is set by return to stop executing code in
 lil_parse, lil_parse_value and loops).
 
   The function will return a LIL value which must be released by calling
 lil_free_value().  The LIL value is the code's result.

   An alternative to lil_parse is lil_parse_value which can be used to
 parse code which is stored in a LIL value.  The signature is
 
     lil_value_t lil_parse_value(lil_t lil,
                                 lil_value_t val,
                                 int funclevel)

   To call a LIl function directly you can use the lil_call function, which
 has the following signature:

     lil_value_t lil_call(lil_t lil,
                          const char* funcname,
                          size_t argc,
                          lil_value_t* argv)

4.3. Raise an error and obtain information about errors
     ----------------------------------------------
   You can raise an error (similar to an exception but all errors in LIL
 are "united" under the same umbrella - runtime errors, syntax errors, etc)
 using the lil_set_error() function which has the following signature
 
     void lil_set_error(lil_t lil,
                        const char* msg)
 
 where msg is the message of the error.  The LIL script can use the "try"
 function to catch the error or, if the error is not catched, the host
 program can use lil_error() to obtain information about it.  The
 lil_error() function will return a non-zero value if there is an error
 message available.  The signature of the function is
 
     int lil_error(lil_t lil,
                   const char** msg,
                   size_t* pos)
 
 the msg pointer will receive the error message and pos will receive the
 position where the message occured (note that the position is usually
 local to the executed command, not the whole script).

4.4. Convert values between C and LIL
     --------------------------------
   LIL uses the lil_value_t type for its own values.  To convert from C to
 lil_value_t you use the following functions:
 
     lil_value_t lil_alloc_string(const char* str)
     lil_value_t lil_alloc_double(double num)
     lil_value_t lil_alloc_integer(lilint_t num)
 
   Note that "alloc" here really means allocate: the values will allocate
 memory on the heap for their contents.  You are responsible for releasing
 the memory using lil_free_value() with the exception of function results
 (see next).
 
   To convert a lil_value_t to a C type use one of the following functions:

     const char* lil_to_string(lil_value_t val)
     double lil_to_double(lil_value_t val)
     lilint_t lil_to_integer(lil_value_t val)
     int lil_to_boolean(lil_value_t val)

   You can also clone a value using lil_clone_value() and append characters
 at the end of existing values using one of the following functions:

     lil_value_t lil_clone_value(lil_value_t src)
     int lil_append_char(lil_value_t val, char ch)
     int lil_append_string(lil_value_t val, const char* s)
     int lil_append_val(lil_value_t val, lil_value_t v)

   Using the following functions you can manipulate LIL lists:

     lil_list_t lil_alloc_list(void)
     void lil_free_list(lil_list_t list)
     void lil_list_append(lil_list_t list, lil_value_t val)
     size_t lil_list_size(lil_list_t list)
     lil_value_t lil_list_get(lil_list_t list, size_t index)
     lil_value_t lil_list_to_value(lil_list_t list, int do_escape)

 the last function will convert a lil_list_t to a LIL value.  Unless you are
 sure that escaping is not needed, you should always use a non-zero value
 for do_escape.  The reverse can be performed with

     lil_list_t lil_subst_to_list(lil_t lil, lil_value_t code)
 
 and combining the two with 

     lil_value_t lil_subst_to_value(lil_t lil, lil_value_t code)

4.5. Register native functions
     ----------------------------
   To register a new function in LIL use the lil_register() function.  This
 function has the following signature:

     int lil_register(lil_t lil,
                      const char* name,
                      lil_func_proc_t proc)

 and returns a non-zero value if the registration was successful.  The
 native function must have the following signature:
 
     lil_value_t lil_func_proc(lil_t lil,
                               size_t argc,
                               lil_value_t* argv)
 
 where lil is the lil object that issued the function, argc is the number
 of arguments used and argv is an array of "argc" lil_value_t elements with
 the arguments passed to the function.  The function must return a newly
 allocated lil_value_t object or NULL if the function returns nothing (or
 returns an empty string).
 
   An example:
   
     static lil_value_t fnc_hi(lil_t lil, size_t argc, lil_value_t* argv)
     {
         printf("Hi!\n");
         return NULL;
     }
 
     ...
     
     lil_register(lil, "hi", fnc_hi);

4.6. Other LIL library functions
     ---------------------------
   Some other library functions are
   
     lil_var_t lil_set_var(lil_t lil,
                           const char* name,
                           lil_value_t val,
                           int local)

 which can be used to set a variable to a value.  local must be one of
 LIL_SETVAR_LOCAL, LIL_SETVAR_LOCAL_NEW, LIL_SETVAR_LOCAL_ONLY or
 LIL_SETVAR_GLOBAL.  Usually you need LIL_SETVAR_LOCAL or LIL_SETVAR_GLOBAL.
 LIL_SETVAR_LOCAL_NEW will always allocate a new variable in the environment
 and should be used with lil_push_env() and lil_pop_env(). See lil.c for
 details on how to use these.

   LIL_SETVAR_LOCAL_ONLY will make sure that the variable is stored in the
 local environment and can be used to avoid name collisions between variables
 defined inside and outside functions.  It should be used by functions that
 set their own variables to be used by the scripts (for example, foreach uses
 this to avoid name collisions between its index variable and the global
 environment).  When in doubt, use LIL_SETVAR_LOCAL if the variable's name is
 provided by the script and the function is supposed to act on a variable,
 otherwise use LIL_SETVAR_LOCAL_ONLY if the variable name is decided by the
 function (with optional alteration of the name by the script, such as in
 foreach) and the variable is to be reused (so LIL_SETVAR_LOCAL_NEW is not a
 good candidate).
 
     lil_value_t lil_get_var(lil_t lil,
                             const char* name)
     
 which can be used to return the value of a variable.  Alternatively
     
     lil_value_t lil_get_var_or(lil_t lil,
                                const char* name,
                                lil_value_t defvalue)

 which will return "defvalue" if the variable is not found.  These two will
 always use the "current" environment (which will be a local environment if
 the function is called from a native function callback for a native
 function called from a script function).

     lil_value_t lil_eval_expr(lil_t lil,
                               lil_value_t code)

 which can be used to evaluate an expression like the "expr" function.
 
     lil_value_t lil_unused_name(lil_t lil,
                                 const char* part)
 
 which can be used to return an unused LIL name using the given part.
 
   Sometimes you need to associate some piece of data (like an object or a
 script name) with a LIL runtime.  You can use the following functions to
 associate a pointer to a lil_t object and retrieve it:
 
     void lil_set_data(lil_t lil,
                       void* data)
  
  which associates "data" with the given lil object and
  
     void* lil_get_data(lil_t lil)
  
  which returns the associated data with the given lil object.  By default
  this will return NULL if no call to lil_set_data() is made.

   LIL can be used as a preprocessor inside another language and/or text in
 a similar way to how PHP is used to generate/preprocess HTML code using the
 lil_embedded like:

     char* lil_embedded(lil_t lil,
                        const char* code,
                        unsigned int flags)

 which returns the "code" with all embedded uses of LIL converted to the
 appropriate code.  The embedded LIL code can access any function inside the
 given lil runtime and temporarily replaces the write and print callbacks to
 emit code that will be returned from the function.  The flags argument is
 currently ignored but for forward compatibility it must be LIL_EMBED_NOFLAGS.
 As an example the text:

     Hello, <?lil write world! ?>

 will return "Hello, world!".  The returned string must be released using the
 lil_freemem function:

     void lil_freemem(void* ptr)

4.7. LIL callback summary
     --------------------
   Here is a summary of the LIL_CALLBACK_* constants for use with the
 lil_callback() function:

     LIL_CALLBACK_EXIT
       callback:  lil_exit_callback_proc_t
       signature: void (lil_t lil, lil_value_t arg)
       called:    when the LIL "exit" function is called
       
     LIL_CALLBACK_WRITE:
       callback:  lil_write_callback_proc_t
       signature: void (lil_t lil, const char* msg)
       called:    when the "print" or "write" function is called.  It is
                  expected to parse the newline character \n
     
     LIL_CALLBACK_READ:
       callback:  lil_read_callback_proc_t
       signature: char* (lil_t lil, const char* name)
       called:    to read the file "name" and return its contents
     
     LIL_CALLBACK_SOURCE:
       callback:  lil_source_callback_proc_t
       signature: char* (lil_t lil, const char* name)
       called:    to read the LIL source file "name" and return its code
     
     LIL_CALLBACK_STORE:
       callback:  lil_store_callback_proc_t
       signature: void (lil_t lil, const char* name, const char* data)
       called:    to store "data" in the file "name" as its contents

     LIL_CALLBACK_ERROR:
       callback:  lil_error_callback_proc_t
       signature: void (lil_t lil, size_t pos, const char* msg)
       called:    when an error occurs and is not handled
     
     LIL_CALLBACK_SETVAR:
       callback:  lil_setvar_callback_proc_t
       signature: int (lil_t lil, const char* name, lil_value_t* value)
       called:    when a non-existant variable is assigned in the global
                  environment.  If this returns a negative value, the
                  assignment does not occur.  If this returns a zero, the
                  value originally to be written is assigned to the
                  variable.  If this returns a positive value, the
                  value pointed by "value" is assigned.  This can be used
                  to override or cancel the assignment of a variable
     
     LIL_CALLBACK_GETVAR:
       callback:  lil_getvar_callback_proc_t
       signature: int (lil_t lil, const char* name, lil_value_t* value)
       called:    when a variable is to be read from the global environment.
                  If this function returns a non-zero value, the value
                  pointed to by "value" is used instead of the value that
                  would be normally returned.  Otherwise the original value
                  is used

4.8. Using LIL as a DLL
     ------------------
   The C code of LIL was not written to be used as a DLL, so keep that in
 mind when you decide to use LIL via a DLL.  Using LIL as a DLL requires a
 couple of extra steps to be performed:
 
     * LIL itself and every code that includes lil.h must be compiled with
       the LILDLL conditional defined.  This will cause all public functions
       to be marked as DLL exportable/importable and use the stdcall
       calling convention.  Note that because by default if no LILDLL is
       defined the library will use the compiler's default calling
       convention you shouldn't mix LILDLL with non-LILDLL sources
     * The LILCALLBACK macro must be used for callbacks.  For example this
     
           static lil_value_t fnc_hi(lil_t lil,
                                     size_t argc,
                                     lil_value_t* argv)
       
       must be changed to this

           static LILCALLBACK lil_value_t fnc_hi(lil_t lil,
                                                 size_t argc,
                                                 lil_value_t* argv)
 
   Under the "dll" directory there is an OpenWatcom project file which will
 create a lil.dll file you can use.  Note that it is highly recommended to
 not mix different DLL versions.  The LIL API and binary interface is not
 frozen and the DLL interface might change.
 
 
5. Integrating LIL in non-C programs
------------------------------------
   It is possible to use LIL from non-C programs as long as a method to
 link against C is provided and the language supports the data types used
 by the public LIL interface (only data types are needed, the public
 interface does not use C structs).  Currently under Windows it is
 recommended to use the DLL version of LIL built using the OpenWatcom
 project found under the "dll" directory of the LIL repository (although it
 might be possible to build a compatible version with MinGW or Visual C++,
 i have not checked this).
 
   While you can write the interface yourself, there are some prewritten
 interfaces which you can find at the Import LIL project at:
 
     https://github.com/badsector/implil
     
   Currently the following languages/environments are supported:
   
     * Visual C# / .NET 4
       (will probably work under older versions, might work under Mono)
     * Free Pascal (and Lazarus)
       
   Check the above site for further and up-to-date information (might list
 other supported interfaces).
 

6. Contact
----------
  Kostas Michalopoulos
  badsector@runtimelegend.com
  badsectoracula@gmail.com
  
  also see http://runtimelegend.com/rep/lil

