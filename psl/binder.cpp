// binder sample
#include "PSL/PSL.h"

#include <stdio.h>

void cfunc(int i)
{
	printf("%d\n", i);
}

double rfunc(int i)
{
	return i;
}

class MyClass
{
public:
	MyClass(int i)			{member = i;}
	int getInt()			{return member;}
	double getFloat()		{return member;}
	void setMember(int i)	{member = i;}
private:
	int member;
};

int main(void)
{
	using namespace PSL;
	PSLVM p;
	if (p.loadScript("sample/binder.psl"))
	{
		printf(" - compile error\n");
		return 1;
	}

	MyClass instance(1024);
	p.addFunction("cfunc", &cfunc)
		("rfunc", &rfunc);
	p.addClass<MyClass>("MyClass")
		("getInt", &MyClass::getInt)
		("getFloat", &MyClass::getFloat)
		("setMember", &MyClass::setMember)
		.instance("instance", &instance);
	p.run();

	return 0;
}
