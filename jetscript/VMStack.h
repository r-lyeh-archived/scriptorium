
#ifndef _VMSTACK
#define _VMSTACK

#include "JetExceptions.h"

namespace Jet
{
	template<class T>
	class VMStack
	{
		const char* overflow_error;
		unsigned int _size;
		unsigned int _max;
	public:
		T mem[5000];
		VMStack()
		{
			overflow_error = "Stack Overflow";
			_size = 0;
			_max = 5000;
			//mem = new T[size];
		}

		VMStack(unsigned int size)
		{
			_size = 0;
			_max = size;
			overflow_error = "Stack Overflow";
			//mem = new T[size];
		}

		VMStack(unsigned int size, const char* error)
		{
			_size = 0;
			_max = size;
			overflow_error = error;
			//mem = new T[size];
		}

		VMStack<T> Copy()
		{
			VMStack<T> ns;
			for (unsigned int i = 0; i < this->_size; i++)
				ns.mem[i] = this->mem[i];

			ns._size = this->_size;
			return std::move(ns);
		}

		~VMStack()
		{
			//delete[] this->mem;
		}

		T Pop()
		{
			if (_size == 0)
				throw RuntimeException("Tried to pop empty stack!");
			return mem[--_size];
		}

		void QuickPop(unsigned int times = 1)
		{
			if (this->_size < times)
				throw RuntimeException("Tried to pop empty stack!");
			_size -= times;
		}

		T& operator[](unsigned int pos)
		{
			if (pos >= this->_max)
				throw RuntimeException("Bad Stack Index");

			return mem[pos];
		}

		T Peek()
		{
			return mem[_size-1];
		}

		void Push(T item)
		{
			if (_size >= _max)
				throw RuntimeException(overflow_error);

			mem[_size++] = item;
		}

		unsigned int size()
		{
			return _size;
		}
	};
}
#endif