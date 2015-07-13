#ifndef __OS_HEAP_MANAGER_H__
#define __OS_HEAP_MANAGER_H__

/******************************************************************************
* Copyright (C) 2012-2014 Evgeniy Golovin (evgeniy.golovin@unitpoint.ru)
*
* Please feel free to contact me at anytime, 
* my email is evgeniy.golovin@unitpoint.ru, skype: egolovin
*
* Latest source code: https://github.com/unitpoint/objectscript
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************/

#include "objectscript.h"

namespace ObjectScript {

	/**
	* \page memorymanager Memory management
	* \section Overview Overview
	* Efficient memory manager.
	*
	* \section tuning Symbols for configuring memory manager
	* 
	* \li \c OS_HEAP_CHUNK_SIZE
	* defines size of chunk where each of objects with size <=
	* OS_HEAP_CHUNK_SIZE is stored. Value must be a power of two.  Maximum
	* recommended value is 256.  Defaults are 128 or 256 depending on memory
	* allocation strategy (see OS_USE_HEAP_SAVING_MODE).
	*
	* \li \c OS_HEAP_PAGE_SIZE
	* is used to define size of page where medium sized objects are stored.
	* Value must be a power of two. Maximum recommended value is 65536. After
	* exhausting existing page, new one with size OS_HEAP_PAGE_SIZE is
	* allocated from system.  Default value is 32 kbytes.
	*
	* \li \c OS_DEBUG
	* enables debugging mode in allocator. It uses more memory, but enables
	* memory overrun and heap corruption detection.
	*
	* \li \c OS_USE_HEAP_SAVING_MODE
	* enables heap-saving mode where pages are not used. But enabling this
	* results in performance drop.
	*
	* \li \c DEBUG_APP_HEAP_DUMP_LEAK_ON_EXIT
	* enables creation of log-file named "dump-err-exit.log" on application
	* exit. File contains information about memory leaks.
	*/

#ifndef OS_HEAP_CHUNK_SIZE
#ifdef OS_USE_HEAP_SAVING_MODE
#define OS_HEAP_CHUNK_SIZE (256 * sizeof(void*)/4)
#else
#define OS_HEAP_CHUNK_SIZE (128 * sizeof(void*)/4)
#endif // OS_USE_HEAP_SAVING_MODE
#endif // OS_HEAP_CHUNK_SIZE

#ifndef OS_HEAP_PAGE_SIZE
#if defined(OS_HEAP_PAGE_SIZE_8K)

#define OS_HEAP_PAGE_SIZE (1024 * 8)

#elif defined(OS_HEAP_PAGE_SIZE_16K)

#define OS_HEAP_PAGE_SIZE (1024 * 16)

#elif defined(OS_HEAP_PAGE_SIZE_32K)

#define OS_HEAP_PAGE_SIZE (1024 * 32)

#elif defined(OS_HEAP_PAGE_SIZE_64K)

#define OS_HEAP_PAGE_SIZE (1024 * 64)

#else

#define OS_HEAP_PAGE_SIZE (1024 * 32 * sizeof(void*)/4)

#endif
#endif // OS_HEAP_PAGE_SIZE

#ifdef OS_EMSCRIPTEN
#define OS_HEAP_ALIGN ((int)sizeof(double))
#else
#define OS_HEAP_ALIGN ((int)sizeof(void*))
#endif

#define OS_HEAP_SIZE_ALIGN(size) (((int)(size)+(OS_HEAP_ALIGN-1))&~(OS_HEAP_ALIGN-1))
#define OS_SIZEOF_INT OS_HEAP_SIZE_ALIGN(sizeof(int))
#define OS_STRUCT_OFFS(s, offs) (OS_BYTE*)((intptr_t)(s) + OS_HEAP_SIZE_ALIGN(sizeof(*s))*(offs))

#ifdef OS_DEBUG
#define OS_DUMMY_ID_SIZE (OS_SIZEOF_INT*2)
#else
#define OS_DUMMY_ID_SIZE 0
#endif

class OSHeapManager: public OS::MemoryManager
{
protected:

	class SimpleStats
	{
	public:

		OS_U32 alloc_size;
		OS_U32 used_size;
		OS_U32 data_size;

#ifdef OS_DEBUG
		OS_U32 max_used_size;
		OS_U32 max_data_size;
		OS_U32 min_block_data_size;
		OS_U32 max_block_data_size;
#endif

		OS_U32 alloc_count;
		OS_U32 free_count;
		OS_U32 hit_count;

	protected:

		friend class OSHeapManager;

		void registerAlloc(OS_U32 used_size, OS_U32 data_size);
		void registerFree(OS_U32 used_size, OS_U32 data_size);
	};

	static int ceilPowerOfTwo(int x);

public:

	struct Stats: public SimpleStats
	{
		OS_U32 merge_count;
		OS_U32 free_page_count;
	};

public:

	enum 
	{
		ALIGN = OS_HEAP_ALIGN,
		MAX_SMALL_SIZE = OS_HEAP_CHUNK_SIZE,
		DEF_PAGE_SIZE = OS_HEAP_PAGE_SIZE
	};

#ifndef OS_USE_HEAP_SAVING_MODE
	enum BlockType
	{
		BT_SMALL,
		BT_MEDIUM,
		BT_LARGE
	};
#else // OS_USE_HEAP_SAVING_MODE
	enum
	{
		BLOCK_TYPE_MASK = 0x80
	};
#endif // OS_USE_HEAP_SAVING_MODE

protected:

	OS_U32 page_size, small_page_size;

#ifndef OS_USE_HEAP_SAVING_MODE
	OS_U32 medium_size_mask;
#endif

	struct SmallBlock
	{
#ifdef OS_DEBUG
		SmallBlock * prev, * next;

		const char * filename;
		int line;
		int id;

		void resetLink();
		void removeLink();
		void insertAfter(SmallBlock * block);
		void insertBefore(SmallBlock * block);
#endif
		OS_BYTE size_slot;
		OS_BYTE pad[sizeof(void*)-1];
		// last byte for type

		OS_U32 getSize() const;
		OS_U32 getDataSize() const;
	};

	struct FreeSmallBlock
	{
		FreeSmallBlock * next;
	};

	struct SmallPage
	{
		OS_U32 size;
		SmallPage * next;
	};

	enum
	{
		SMALL_SLOT_COUNT = (MAX_SMALL_SIZE + sizeof(SmallBlock) + OS_DUMMY_ID_SIZE) / ALIGN + 1
	};

	FreeSmallBlock * free_small_blocks[SMALL_SLOT_COUNT];
	SmallPage dummy_small_page;
	SmallPage * small_page;
	OS_U32 small_page_offs;

#ifdef OS_DEBUG
	SmallBlock dummy_small_block;
	int cur_id;
#endif

	SimpleStats small_stats;

	void * allocSmall(OS_U32 size OS_DBG_FILEPOS_DECL);
	void freeSmall(void * p);
	OS_U32 getSizeSmall(void * p);

protected:

	struct Block
	{
		Block * prev, * next;

#ifdef OS_DEBUG
		const OS_CHAR * filename;
		int line;
		int id;
#endif

		OS_U32 size;
		OS_U16 page;
		OS_BYTE is_free;
		OS_BYTE pad0; // for type

		OS_U32 getDataSize() const;
		void resetLink();
		void removeLink();
		void insertAfter(Block * block);
		void insertBefore(Block * block);
	};

#ifndef OS_USE_HEAP_SAVING_MODE
	struct FreeBlock: public Block
	{
#ifdef OS_DEBUG
		int pad0; // for DUMMY_MEDIUM_FREE_ID_PRE
#endif
		FreeBlock * prev_free, * next_free;

		void resetFreeLink();
		void removeFreeLink();
		void insertAfterFreeLink(FreeBlock * block);
		void insertBeforeFreeLink(FreeBlock * block);
	};

	Block dummy_block;
	FreeBlock dummy_free;
	OS_U16 next_page;

	Stats medium_stats;

	void * allocMedium(OS_U32 size OS_DBG_FILEPOS_DECL);
	void freeMedium(void * p);
	OS_U32 getSizeMedium(void * p);

	void insertFreeBlock(FreeBlock * block);

protected:

#endif // OS_USE_HEAP_SAVING_MODE

	Block dummy_large_block;
	SimpleStats large_stats;

	void * allocLarge(OS_U32 size OS_DBG_FILEPOS_DECL);
	void freeLarge(void * p);
	OS_U32 getSizeLarge(void * p);

	void writeFile(OS * os, OS::FileHandle * f, const char * buf);

	void writeStats(OS * os, OS::FileHandle * f);

#ifdef OS_DEBUG
	void writeSmallBlockHeader(OS * os, OS::FileHandle * f);
	void writeSmallBlock(OS * os, OS::FileHandle * f, SmallBlock * block);
	void writeSmallBlocks(OS * os, OS::FileHandle * f);
#endif

	void writeBlockHeader(OS * os, OS::FileHandle * f, int type);
	void writeBlock(OS * os, OS::FileHandle * f, Block * block);
	void writeBlocks(OS * os, OS::FileHandle * f, Block * dummy_block);

#ifndef OS_USE_HEAP_SAVING_MODE
	void writeFreeBlockHeader(OS * os, OS::FileHandle * f);
	void writeFreeBlocks(OS * os, OS::FileHandle * f);
#endif // OS_USE_HEAP_SAVING_MODE

protected:

	virtual ~OSHeapManager();

public:

	OSHeapManager();

	virtual void * malloc(int size OS_DBG_FILEPOS_DECL);
	virtual void free(void * p);
		
	// void * realloc(void * p OS_DBG_FILEPOS_DECL);

	virtual void setBreakpointId(int id);

	virtual int getAllocatedBytes();
	virtual int getMaxAllocatedBytes();
	virtual int getUsedBytes();
	virtual int getCachedBytes();

	OS_U32 getSize(void * p);

	OS_U32 getPageSize() const;
	// void SetPageSize(OS_U32 value);

	void dumpUsage(OS * os, const OS_CHAR * filename);

	void getStats(Stats& small_stats, Stats& medium_stats, Stats& large_stats);

	struct SummaryStats: public Stats
	{
#ifdef OS_DEBUG
		OS_U32 min_small_block_data_size;
		OS_U32 max_small_block_data_size;

		OS_U32 min_medium_block_data_size;
		OS_U32 max_medium_block_data_size;

		OS_U32 min_large_block_data_size;
		OS_U32 max_large_block_data_size;
#endif
	};

	void getStats(SummaryStats& stats);

	void checkMemory();
};

}; // namespace ObjectScript

#endif // __OS_HEAP_MANAGER_H__
