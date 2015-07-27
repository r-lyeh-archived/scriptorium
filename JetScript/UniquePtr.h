#ifndef UNIQUE_POINTER_HEADER
#define UNIQUE_POINTER_HEADER

namespace Jet
{
	template <class T>
	class UniquePtr
	{
		T ptr;
	public:
		inline UniquePtr()
		{
			ptr = 0;
		}

		inline UniquePtr(T pointer)
		{
			ptr = pointer;
		}

		inline UniquePtr(UniquePtr<T> && other)
		{
			ptr = other.ptr;
			other.ptr = 0;
		}

		inline ~UniquePtr()
		{
			delete ptr;
		}

		inline operator T()
		{
			return ptr;
		}
		
		inline T operator ->()
		{
			return ptr;
		}

		inline UniquePtr<T> operator = (const T p)
		{
			ptr = p;
			return p;
		}

		//returns the pointer and gets done with this
		inline T Release()
		{
			auto temp = ptr;
			ptr = 0;
			return temp;
		}
	};

	template <class T>
	class UniquePtr<T[]>
	{
		typedef T type;
		T* ptr;
	public:
		inline UniquePtr()
		{
			ptr = 0;
		}

		inline UniquePtr(T* pointer)
		{
			ptr = pointer;
		}

		inline UniquePtr(UniquePtr<T> && other)
		{
			ptr = other.ptr;
			other.ptr = 0;
		}

		inline ~UniquePtr()
		{
			delete[] ptr;
		}

		inline operator T*()
		{
			return ptr;
		}

		//T& operator[](unsigned int id) const
		//{
		//	return ptr[id];
		//}
	};
}
#endif