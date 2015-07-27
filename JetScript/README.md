 Jet
==========

An easy to integrate scripting language written in C++.

See the wiki for more detailed info!

[Try Jet!](https://dl.dropboxusercontent.com/u/31914262/jet_testbox/jet_test.html)

### Example
```cpp
fun fibo(n)
{
	if (n < 2)
		return n;
	else
		return fibo(n-1)+fibo(n-2);
}
print("Fibonacci of 10: ", fibo(10));
```

### General Concepts:

Everything is a global variable unless the local keyword is placed before it.

### How to use in your program:
```cpp
#include <JetContext.h>

Jet::JetContext context;
try
{
	Jet::Value return = context.Script("return 7;");
}
catch (CompilerException e)
{
	//an exception occured while compiling
}
```

### Types
- Numbers
```cpp
number = 256;
number = 3.1415926535895;
```
- Strings
```cpp
string = "hello";
```
- Objects - a map like type
```cpp
//how to define objects
obj = {};
obj2 = { hey = 1, apple = 2 };
//two different ways to index items in the object
obj.apple = 2;
obj["apple2"] = 3;
```
- Arrays - a contiguous array of values with a set size
```cpp
//how to define arrays
arr = [];
arr2 = [1,2,3,4,5,6];

arr:resize(2);
arr[0] = 255;
arr[1] = "Apples";
```
- Userdata - used for native user defined types, stores a void*

### C++ Bindings
You can bind functions in C++ to Jet as well as bind native types through  userdata types and metatables.

- Binding Functions
```cpp
Jet::JetContext context;
context["myfunction"] = [](JetContext* context, Value* arguments, int numarguments)
{
	printf("Hello from C++!");
};
context.Script("myfunction();");
```
Outputs "Hello from C++!" to console.


- Creating Userdata
```cpp
Jet::JetContext context;
auto meta = context.NewPrototype("TypeName");//this is a list of all meta-methods you want to add
meta["t1"] = [](JetContext* context, Value* v, int args)
{
	printf("Hi from metatable!");
};

context["x"] = context.NewUserdata(0/*any native data you want associated*/, meta);
auto out = context.Script("x.t1();");
```
Outputs "Hi from metatable!" to the console.