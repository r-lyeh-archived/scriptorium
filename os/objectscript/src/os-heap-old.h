#ifndef __OS_HEAP_MANAGER_OLD_H__
#define __OS_HEAP_MANAGER_OLD_H__

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

#ifndef OS_MEMORY_MANAGER_PAGE_BLOCKS
#define OS_MEMORY_MANAGER_PAGE_BLOCKS 32
#endif

namespace ObjectScript {

class OSMemoryManagerOld: public OS::MemoryManager
{
protected:

	int allocated_bytes;
	int max_allocated_bytes;
	int cached_bytes;

	struct PageDesc
	{
		int block_size;
		int num_blocks;

		int allocated_bytes;
	};

	struct Page
	{
		int index;
		int num_cached_blocks;
		Page * next_page;
	};

	struct CachedBlock
	{
#ifdef OS_DEBUG
		int mark;
#endif
		Page * page;
		CachedBlock * next;
	};

	struct MemBlock
	{
		Page * page;
#ifdef OS_DEBUG
		const OS_CHAR * dbg_filename;
		int dbg_line;
		int dbg_id;
		MemBlock * dbg_mem_prev;
		MemBlock * dbg_mem_next;
#endif
		int block_size;
#ifdef OS_DEBUG
		int mark;
#endif
	};

	struct StdMemBlock
	{
#ifdef OS_DEBUG
		const OS_CHAR * dbg_filename;
		int dbg_line;
		int dbg_id;
		StdMemBlock * dbg_mem_prev;
		StdMemBlock * dbg_mem_next;
#endif
		int block_size;
#ifdef OS_DEBUG
		int mark;
#endif
	};

	enum {
		MAX_PAGE_TYPE_COUNT = 17
	};

	PageDesc page_desc[MAX_PAGE_TYPE_COUNT];
	int num_page_desc;

	int * page_map;
	int page_map_size;

	Page * pages[MAX_PAGE_TYPE_COUNT];

	CachedBlock * cached_blocks[MAX_PAGE_TYPE_COUNT];

#ifdef OS_DEBUG
	MemBlock * dbg_mem_list;
	StdMemBlock * dbg_std_mem_list;
	int dbg_breakpoint_id;
#endif

	int stat_malloc_count;
	int stat_free_count;

	void registerPageDesc(int block_size, int num_blocks);
			
	static int comparePageDesc(const void * pa, const void * pb);
	void sortPageDesc();

	void * allocFromCachedBlock(int i OS_DBG_FILEPOS_DECL);
	void * allocFromPageType(int i OS_DBG_FILEPOS_DECL);
	void freeMemBlock(MemBlock*);

	void freeCachedMemory(int new_cached_bytes);

	void * stdAlloc(int size OS_DBG_FILEPOS_DECL);
	void stdFree(void * p);

public:

	OSMemoryManagerOld();
	~OSMemoryManagerOld();
			
	void * malloc(int size OS_DBG_FILEPOS_DECL);
	void free(void * p);
	void setBreakpointId(int id);

	int getAllocatedBytes();
	int getMaxAllocatedBytes();
	int getUsedBytes();
	int getCachedBytes();
};


}; // namespace ObjectScript

#endif // __OS_HEAP_MANAGER_OLD_H__
