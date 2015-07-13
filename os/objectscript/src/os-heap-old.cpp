#include "os-heap-old.h"

#include <malloc.h>
#include <string.h>

using namespace ObjectScript;

OSMemoryManagerOld::OSMemoryManagerOld()
{
	allocated_bytes = 0;
	max_allocated_bytes = 0;
	cached_bytes = 0;
	OS_MEMSET(page_desc, 0, sizeof(page_desc));
	num_page_desc = 0;
	OS_MEMSET(pages, 0, sizeof(pages));
	OS_MEMSET(cached_blocks, 0, sizeof(cached_blocks));

#ifdef OS_DEBUG
	dbg_mem_list = NULL;
	dbg_std_mem_list = NULL;
	dbg_breakpoint_id = -1;
#endif

	stat_malloc_count = 0;
	stat_free_count = 0;

	registerPageDesc(sizeof(OS::Core::GCObjectValue), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::GCStringValue), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::GCUserdataValue), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::GCFunctionValue), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::GCCFunctionValue), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::GCCFunctionValue) + sizeof(OS::Core::Value)*4, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::Property), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	// registerPageDesc(sizeof(OS::Core::StackFunction), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::Locals), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::Locals) + sizeof(void*)*4, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::Locals) + sizeof(void*)*8, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::Table), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::Compiler::EXPRESSION_SIZE), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(sizeof(OS::Core::TokenData), OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(8, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(16, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(32, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(64, OS_MEMORY_MANAGER_PAGE_BLOCKS);
	registerPageDesc(128, OS_MEMORY_MANAGER_PAGE_BLOCKS/2);
	registerPageDesc(256, OS_MEMORY_MANAGER_PAGE_BLOCKS/4);
	sortPageDesc();

	page_map_size = page_desc[num_page_desc-1].block_size + 1;
	page_map = new int[page_map_size];
	for(int i = 0; i < page_map_size; i++){
		page_map[i] = -1;
	}
}

OSMemoryManagerOld::~OSMemoryManagerOld()
{
	freeCachedMemory(0);
	delete [] page_map;
#ifdef OS_DEBUG
	{
		for(MemBlock * mem = dbg_mem_list; mem; mem = mem->dbg_mem_next){
			OS_PRINTF("[LEAK] %d bytes, id: %d, line %d, %s\n", mem->block_size, mem->dbg_id, mem->dbg_line, mem->dbg_filename);
		}
	}
	{
		for(StdMemBlock * mem = dbg_std_mem_list; mem; mem = mem->dbg_mem_next){
			OS_ASSERT(mem->block_size & 0x80000000);
			OS_PRINTF("[LEAK] %d bytes, id: %d, line %d, %s\n", (mem->block_size & ~0x80000000), mem->dbg_id, mem->dbg_line, mem->dbg_filename);
		}
	}
#endif
	// OS_ASSERT(!allocated_bytes && !cached_bytes);
}

#ifdef OS_DEBUG
static const int MEM_MARK_BEGIN = 0xabcdef98;
static const int MEM_MARK_END = 0x3579faec;
static const int FREE_MARK_BEGIN = 0xdabcef98;
static const int FREE_MARK_END = 0x3faec579;
static const int STD_MEM_MARK_BEGIN = 0xaefbcd98;
static const int STD_MEM_MARK_END = 0x35ae79fc;
#define MEM_MARK_END_SIZE sizeof(int)
#else
#define MEM_MARK_END_SIZE 0
#endif

int OSMemoryManagerOld::comparePageDesc(const void * pa, const void * pb)
{
	PageDesc * a = (PageDesc*)pa;
	PageDesc * b = (PageDesc*)pb;
	return a->block_size - b->block_size;
}

void OSMemoryManagerOld::sortPageDesc()
{
	::qsort(page_desc, num_page_desc, sizeof(page_desc[0]), comparePageDesc);
}

void OSMemoryManagerOld::registerPageDesc(int block_size, int num_blocks)
{
	if(num_page_desc == MAX_PAGE_TYPE_COUNT){
		return;
	}
	if(block_size > 128){
		block_size = (block_size + 31) & ~31;
	}else if(block_size > 64){
		block_size = (block_size + 15) & ~15;
	}else if(block_size > 32){
		block_size = (block_size + 7) & ~7;
	}else{
		block_size = (block_size + 3) & ~3;
	}
	int i;
	for(i = 0; i < num_page_desc; i++){
		if(page_desc[i].block_size == block_size){
			if(page_desc[i].num_blocks < num_blocks){
				page_desc[i].num_blocks = num_blocks;
				page_desc[i].allocated_bytes = sizeof(Page) + (sizeof(MemBlock) + block_size + MEM_MARK_END_SIZE) * num_blocks;
			}
			return;
		}
	}
	page_desc[i].block_size = block_size;
	page_desc[i].num_blocks = num_blocks;
	page_desc[i].allocated_bytes = sizeof(Page) + (sizeof(MemBlock) + block_size + MEM_MARK_END_SIZE) * num_blocks;
	num_page_desc++;
}

void * OSMemoryManagerOld::allocFromCachedBlock(int i OS_DBG_FILEPOS_DECL)
{
#ifdef OS_DEBUG
	if(stat_malloc_count == dbg_breakpoint_id){
		DEBUG_BREAK;
	}
#endif
	stat_malloc_count++;
	OS_ASSERT(i >= 0 && i < num_page_desc);
	CachedBlock * cached_block = cached_blocks[i];
	OS_ASSERT(cached_block);
#ifdef OS_DEBUG
	OS_ASSERT(cached_block->mark == FREE_MARK_BEGIN);
	OS_ASSERT(*(int*)(((OS_BYTE*)((MemBlock*)cached_block+1)) + page_desc[i].block_size) == FREE_MARK_END);
#endif
	cached_blocks[i] = cached_block->next;
	Page * page = cached_block->page;
	OS_ASSERT(page->num_cached_blocks > 0);
	page->num_cached_blocks--;
	MemBlock * mem_block = (MemBlock*)cached_block;
	mem_block->page = page;
	mem_block->block_size = page_desc[i].block_size;
#ifdef OS_DEBUG
	mem_block->mark = MEM_MARK_BEGIN;
	*(int*)(((OS_BYTE*)(mem_block+1)) + mem_block->block_size) = MEM_MARK_END;

	mem_block->dbg_filename = dbg_filename;
	mem_block->dbg_line = dbg_line;
	mem_block->dbg_id = stat_malloc_count-1;

	mem_block->dbg_mem_prev = NULL;
	mem_block->dbg_mem_next = dbg_mem_list;
	if(dbg_mem_list){
		dbg_mem_list->dbg_mem_prev = mem_block;
	}
	dbg_mem_list = mem_block;
#endif
	cached_bytes -= mem_block->block_size + sizeof(MemBlock);
	void * p = mem_block + 1;
	OS_MEMSET(p, 0, mem_block->block_size);
	// OS_ASSERT(mem_block->mark == MEM_MARK_BEGIN);
	// OS_ASSERT(*(int*)(((OS_BYTE*)(mem_block+1)) + mem_block->block_size) == MEM_MARK_END);
	return p;
}

void * OSMemoryManagerOld::allocFromPageType(int i OS_DBG_FILEPOS_DECL)
{
	OS_ASSERT(i >= 0 && i < num_page_desc);
	if(cached_blocks[i]){
		return allocFromCachedBlock(i OS_DBG_FILEPOS_PARAM);
	}

	int allocated_bytes = page_desc[i].allocated_bytes;
	Page * page = (Page*)stdAlloc(allocated_bytes OS_DBG_FILEPOS);
	page->index = i;
	page->next_page = pages[i];
	pages[i] = page;
	page->num_cached_blocks = page_desc[i].num_blocks;
	cached_bytes += allocated_bytes;

	OS_BYTE * next_page_block = (OS_BYTE*)(page + 1);
	for(int j = 0; j < page_desc[i].num_blocks; j++){
		CachedBlock * cached_block = (CachedBlock*)next_page_block;
		cached_block->page = page;
		cached_block->next = cached_blocks[i];
#ifdef OS_DEBUG
		cached_block->mark = FREE_MARK_BEGIN;
		*(int*)(((OS_BYTE*)((MemBlock*)cached_block+1)) + page_desc[page->index].block_size) = FREE_MARK_END;
		OS_MEMSET(cached_block+1, 0xde, page_desc[i].block_size + (sizeof(MemBlock) - sizeof(CachedBlock)));
#endif
		cached_blocks[i] = cached_block;
		next_page_block += sizeof(MemBlock) + page_desc[i].block_size + MEM_MARK_END_SIZE;
	}

	return allocFromCachedBlock(i OS_DBG_FILEPOS_PARAM);
}

void OSMemoryManagerOld::freeMemBlock(MemBlock * mem_block)
{
	stat_free_count++;
#ifdef OS_DEBUG
	OS_ASSERT(mem_block->mark == MEM_MARK_BEGIN);
	OS_ASSERT(*(int*)(((OS_BYTE*)(mem_block+1)) + mem_block->block_size) == MEM_MARK_END);
	if(mem_block->dbg_id == dbg_breakpoint_id){
		DEBUG_BREAK;
	}
	if(mem_block == dbg_mem_list){
		OS_ASSERT(!mem_block->dbg_mem_prev);
		dbg_mem_list = mem_block->dbg_mem_next;
	}else{ // if(mem_block->dbg_mem_prev){
		OS_ASSERT(mem_block->dbg_mem_prev);
		mem_block->dbg_mem_prev->dbg_mem_next = mem_block->dbg_mem_next;
	}
	if(mem_block->dbg_mem_next){
		mem_block->dbg_mem_next->dbg_mem_prev = mem_block->dbg_mem_prev;
	}
#endif
	Page * page = mem_block->page;
	int size = mem_block->block_size;
	cached_bytes += size + sizeof(MemBlock);
	CachedBlock * cached_block = (CachedBlock*)mem_block;
	cached_block->page = page;
	cached_block->next = cached_blocks[page->index];
#ifdef OS_DEBUG
	cached_block->mark = FREE_MARK_BEGIN;
	*(int*)(((OS_BYTE*)((MemBlock*)cached_block+1)) + page_desc[page->index].block_size) = FREE_MARK_END;
	OS_MEMSET(cached_block+1, 0xde, size + (sizeof(MemBlock) - sizeof(CachedBlock)));
#endif
	cached_blocks[page->index] = cached_block;
	page->num_cached_blocks++;
}

void OSMemoryManagerOld::freeCachedMemory(int new_cached_bytes)
{
	if(cached_bytes > new_cached_bytes){
		for(int i = num_page_desc-1; i >= 0; i--){
			bool found_free_page = false;
			int num_blocks = page_desc[i].num_blocks;
			CachedBlock * prev_cached_block = NULL, * next_cached_block = NULL;
			for(CachedBlock * cached_block = cached_blocks[i]; cached_block; cached_block = next_cached_block){
				OS_ASSERT(cached_block->page->index == i);
				next_cached_block = cached_block->next;
				if(cached_block->page->num_cached_blocks == num_blocks){
					found_free_page = true;
					if(!prev_cached_block){
						cached_blocks[i] = next_cached_block;
					}else{
						prev_cached_block->next = next_cached_block;
					}
					// keep prev_cached_block
					continue;
				}
				prev_cached_block = cached_block;
			}
			if(found_free_page){
				Page * prev = NULL, * next;
				for(Page * page = pages[i]; page; page = next){
					next = page->next_page;
					if(page->num_cached_blocks == num_blocks){
						if(!prev){
							pages[i] = page->next_page;
						}else{
							prev->next_page = page->next_page;
						}
						cached_bytes -= page_desc[i].allocated_bytes;
						stdFree(page);
						// stat_free_count++;
					}else{
						prev = page;
					}
				}
				if(cached_bytes <= new_cached_bytes){
					break;
				}
			}
		}
	}
}

void * OSMemoryManagerOld::stdAlloc(int size OS_DBG_FILEPOS_DECL)
{
#ifdef OS_DEBUG
	if(stat_malloc_count == dbg_breakpoint_id){
		DEBUG_BREAK;
	}
#endif
	stat_malloc_count++;
	size = (size + 7) & ~7;
	StdMemBlock * mem_block = (StdMemBlock*)::malloc(size + sizeof(StdMemBlock) + MEM_MARK_END_SIZE);
	if(!mem_block && cached_bytes > 0){
		freeCachedMemory(0);
		mem_block = (StdMemBlock*)::malloc(size + sizeof(StdMemBlock) + MEM_MARK_END_SIZE);
		if(!mem_block){
			return NULL;
		}
	}
#ifdef OS_DEBUG
	mem_block->mark = STD_MEM_MARK_BEGIN;
	*(int*)(((OS_BYTE*)(mem_block+1)) + size) = STD_MEM_MARK_END;

	mem_block->dbg_filename = dbg_filename;
	mem_block->dbg_line = dbg_line;
	mem_block->dbg_id = stat_malloc_count-1;

	mem_block->dbg_mem_prev = NULL;
	mem_block->dbg_mem_next = dbg_std_mem_list;
	if(dbg_std_mem_list){
		dbg_std_mem_list->dbg_mem_prev = mem_block;
	}
	dbg_std_mem_list = mem_block;
#endif
	mem_block->block_size = size | 0x80000000;
	allocated_bytes += size + sizeof(StdMemBlock) + MEM_MARK_END_SIZE;
	if(max_allocated_bytes < allocated_bytes){
		max_allocated_bytes = allocated_bytes;
	}
	OS_MEMSET(mem_block+1, 0, size);
	return mem_block+1;
}

void OSMemoryManagerOld::stdFree(void * ptr)
{
	stat_free_count++;
	StdMemBlock * mem_block = (StdMemBlock*)ptr - 1;
	OS_ASSERT(mem_block->block_size & 0x80000000);
#ifdef OS_DEBUG
	OS_ASSERT(mem_block->mark == STD_MEM_MARK_BEGIN);
	OS_ASSERT(*(int*)(((OS_BYTE*)(mem_block+1)) + (mem_block->block_size & ~0x80000000)) == STD_MEM_MARK_END);

	if(mem_block->dbg_id == dbg_breakpoint_id){
		DEBUG_BREAK;
	}
	if(mem_block == dbg_std_mem_list){
		OS_ASSERT(!mem_block->dbg_mem_prev);
		dbg_std_mem_list = mem_block->dbg_mem_next;
	}else{ // if(mem_block->dbg_mem_prev){
		OS_ASSERT(mem_block->dbg_mem_prev);
		mem_block->dbg_mem_prev->dbg_mem_next = mem_block->dbg_mem_next;
	}
	if(mem_block->dbg_mem_next){
		mem_block->dbg_mem_next->dbg_mem_prev = mem_block->dbg_mem_prev;
	}
#endif
	int size = mem_block->block_size & ~0x80000000;
	allocated_bytes -= size + sizeof(StdMemBlock) + MEM_MARK_END_SIZE;
#ifdef OS_DEBUG
	OS_MEMSET(ptr, 0xde, size);
#endif
	::free(mem_block);
}

void * OSMemoryManagerOld::malloc(int size OS_DBG_FILEPOS_DECL)
{
	if(size <= 0){
		return NULL;
	}
#if 1 // performance optimization
	if(size < page_map_size){
		int i = page_map[size];
		if(i < 0){
			for(i = 0; i < num_page_desc; i++){
				if(size <= page_desc[i].block_size){
					return allocFromPageType(page_map[size] = i OS_DBG_FILEPOS_PARAM);
				}
			}
			OS_ASSERT(false);
		}
		return allocFromPageType(i OS_DBG_FILEPOS_PARAM);
	}
	OS_ASSERT(size > page_desc[num_page_desc-1].block_size);
#else
	if(size <= page_desc[num_page_desc-1].block_size){
		for(int i = 0; i < num_page_desc; i++){
			if(size <= page_desc[i].block_size){
				return allocFromPageType(i OS_DBG_FILEPOS_PARAM);
			}
		}
	}
#endif
	return stdAlloc(size OS_DBG_FILEPOS_PARAM);
}

void OSMemoryManagerOld::free(void * ptr)
{
	if(!ptr){
		return;
	}
	// stat_free_count++;
#ifdef OS_DEBUG
	int * p = (int*)ptr - 2;
#else
	int * p = (int*)ptr - 1;
#endif
	int size = p[0];
	if(size & 0x80000000){
		stdFree(ptr); // p, size & ~0x80000000);
		return;
	}
	MemBlock * mem_block = (MemBlock*)ptr - 1;
	OS_ASSERT(mem_block->block_size == size);
	freeMemBlock(mem_block);
	if(!(stat_free_count % 1024) && cached_bytes > allocated_bytes / 2){
		freeCachedMemory(cached_bytes / 2);
	}
}

void OSMemoryManagerOld::setBreakpointId(int id)
{
#ifdef OS_DEBUG
	dbg_breakpoint_id = id;
#endif
}

int OSMemoryManagerOld::getAllocatedBytes()
{
	return allocated_bytes;
}

int OSMemoryManagerOld::getMaxAllocatedBytes()
{
	return max_allocated_bytes;
}

int OSMemoryManagerOld::getUsedBytes()
{
	return allocated_bytes - cached_bytes;
}

int OSMemoryManagerOld::getCachedBytes()
{
	return cached_bytes;
}
