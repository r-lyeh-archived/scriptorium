#include "os-heap.h"

#ifndef __APPLE__
#include <malloc.h>
#else
#include <malloc/malloc.h>
#endif
#include <string.h>

using namespace ObjectScript;

// #define OS_ASSERT OS_ASSERT
#define STD_MALLOC ::malloc
#define STD_FREE ::free

// #define USE_STD_MALLOC
#define FREE_FREEPAGES
#define FIND_BEST_FREE_BLOCK

#define DEAD_BYTE	0xde

#define DUMMY_SMALL_USED_ID_PRE		0xedededed
#define DUMMY_SMALL_USED_ID_POST	0xdededede

#define DUMMY_SMALL_FREE_ID_PRE		0xed5ded5d
#define DUMMY_SMALL_FREE_ID_POST	0xded5ded5

#define DUMMY_MEDIUM_USED_ID_PRE	0xedcdedcd
#define DUMMY_MEDIUM_USED_ID_POST	0xdedcdedc

#define DUMMY_MEDIUM_FREE_ID_PRE	0xed5cec5d
#define DUMMY_MEDIUM_FREE_ID_POST	0xded5cec5

#define DUMMY_LARGE_USED_ID_PRE  0xedadedad
#define DUMMY_LARGE_USED_ID_POST 0xdedadeda

#define DUMMY_LARGE_FREE_ID_PRE  0xed5aea5d
#define DUMMY_LARGE_FREE_ID_POST 0xded5aea5

#define SAVE_EFS_SIZE (1024*10)

#if defined __GNUC__ || defined IW_SDK

static int OS_VSNPRINTF(OS_CHAR * str, size_t size, const OS_CHAR *format, va_list va)
{
	return vsnprintf(str, size, format, va);
}

#else

static int OS_VSNPRINTF(OS_CHAR * str, size_t size, const OS_CHAR *format, va_list va)
{
	return vsnprintf_s(str, size, size/sizeof(OS_CHAR), format, va);
}

#endif

static int OS_SNPRINTF(OS_CHAR * str, size_t size, const OS_CHAR *format, ...)
{
	va_list va;
	va_start(va, format);
	int ret = OS_VSNPRINTF(str, size, format, va);
	va_end(va);
	return ret;
}

void OSHeapManager::SimpleStats::registerAlloc(OS_U32 used_size, OS_U32 data_size)
{
	alloc_count++;
	this->used_size += used_size;
	this->data_size += data_size;

#ifdef OS_DEBUG
	if(max_used_size < this->used_size){
		max_used_size = this->used_size;
	}
	if(max_data_size < this->data_size){
		max_data_size = this->data_size;
	}

	if(min_block_data_size > data_size){
		min_block_data_size = data_size;
	}
	if(max_block_data_size < data_size){
		max_block_data_size = data_size;
	}
#endif
}

void OSHeapManager::SimpleStats::registerFree(OS_U32 used_size, OS_U32 data_size)
{
	free_count++;
	this->used_size -= used_size;
	this->data_size -= data_size;
}

int OSHeapManager::ceilPowerOfTwo(int x)
{
	x--;
	x |= x >> 1;
	x |= x >> 2;
	x |= x >> 4;
	x |= x >> 8;
	x |= x >> 16;
	x++;
	return x;
}

OS_U32 OSHeapManager::SmallBlock::getSize() const
{
	return size_slot * ALIGN + ALIGN;
}

inline OS_U32 OSHeapManager::SmallBlock::getDataSize() const
{ 
	return size_slot * ALIGN + ALIGN - sizeof(SmallBlock) - OS_DUMMY_ID_SIZE;
}

#ifdef OS_DEBUG
void OSHeapManager::SmallBlock::resetLink()
{
	prev = next = this;
}

void OSHeapManager::SmallBlock::removeLink()
{
	prev->next = next;
	next->prev = prev;
}

void OSHeapManager::SmallBlock::insertAfter(SmallBlock * block)
{
	next = block->next;
	next->prev = this;
	block->next = this;
	prev = block;
}

void OSHeapManager::SmallBlock::insertBefore(SmallBlock * block)
{
	prev = block->prev;
	prev->next = this;
	block->prev = this;
	next = block;
}
#endif

void * OSHeapManager::allocSmall(OS_U32 size OS_DBG_FILEPOS_DECL)
{
	// OS_U32 saveSize = size;
	size = (size + ALIGN - 1 + sizeof(SmallBlock) + OS_DUMMY_ID_SIZE) & ~(ALIGN-1);

	OS_U32 i = size / ALIGN - 1;
	OS_ASSERT(i < SMALL_SLOT_COUNT);

	SmallBlock * small_block;
	FreeSmallBlock * first = free_small_blocks[i];
	if(first){
		small_block = (SmallBlock*)first;
		free_small_blocks[i] = first->next;
		small_stats.hit_count++;
	}else{
		OS_U32 free_page_size = small_page->size - small_page_offs;
		if(free_page_size < size){
			if(free_page_size > ((ALIGN + ALIGN - 1 + sizeof(SmallBlock) + OS_DUMMY_ID_SIZE) & ~(ALIGN-1))){
				OS_U32 i = free_page_size / ALIGN - 1;
				OS_ASSERT(i < SMALL_SLOT_COUNT);

				small_block = (SmallBlock*)(((OS_BYTE*)small_page) + small_page_offs);
				small_block->size_slot = (OS_BYTE)i;

#ifdef OS_DEBUG
				OS_BYTE * p = (OS_BYTE*)(OS_STRUCT_OFFS(small_block, 1));
				*(int*)p = DUMMY_SMALL_FREE_ID_PRE;
				*(int*)(p + small_block->getDataSize() + OS_SIZEOF_INT) = DUMMY_SMALL_FREE_ID_POST;
#endif

				((FreeSmallBlock*)small_block)->next = free_small_blocks[i];
				free_small_blocks[i] = (FreeSmallBlock*)small_block;
			}
			SmallPage * new_small_page = (SmallPage*)STD_MALLOC(small_page_size);
			if(!new_small_page){
				return NULL;
			}
			new_small_page->size = small_page_size;

			small_page_offs = OS_HEAP_SIZE_ALIGN(sizeof(*new_small_page));
			small_stats.alloc_size += new_small_page->size;

			new_small_page->next = small_page;
			small_page = new_small_page;
		}else{
			small_stats.hit_count++;
		}
		OS_ASSERT("Heap corrupted!" && (small_page_offs & (ALIGN-1)) == 0);
		small_block = (SmallBlock*)(((OS_BYTE*)small_page) + small_page_offs);
		small_page_offs += size;
	}

	small_block->size_slot = (OS_BYTE)i;
#ifdef OS_DEBUG
	small_block->filename = dbg_filename;
	small_block->line = dbg_line;
	small_block->insertBefore(dummy_small_block.next);
	small_block->id = cur_id;
#endif
	OS_U32 data_size = small_block->getDataSize();
	small_stats.registerAlloc(small_block->getSize(), data_size);

	OS_BYTE * p = OS_STRUCT_OFFS(small_block, 1);
#ifndef OS_USE_HEAP_SAVING_MODE
	p[-1] = BT_SMALL;
#else
	p[-1] = BLOCK_TYPE_MASK;
#endif

#ifdef OS_DEBUG
	*(int*)p = DUMMY_SMALL_USED_ID_PRE;
	p += OS_SIZEOF_INT;
	*(int*)(p + data_size) = DUMMY_SMALL_USED_ID_POST;
#endif

	OS_ASSERT("Heap align corrupted!" && ((int)(intptr_t)(p) % ALIGN) == 0);
	OS_MEMSET(p, 0, data_size);

#if defined(OS_DEBUG)
	// checkMemory();
#endif

	return p;
}

void OSHeapManager::freeSmall(void * p)
{
	OS_ASSERT("Trying to free NULL pointer" && p);
	OS_ASSERT("Heap corrupted!" && small_stats.alloc_count > small_stats.free_count);
#ifdef OS_DEBUG
	p = (OS_BYTE*)p - OS_SIZEOF_INT;
	OS_ASSERT("Heap corrupted or trying to free alien memory" && *(int*)p == DUMMY_SMALL_USED_ID_PRE);
	*(int*)p = DUMMY_SMALL_FREE_ID_PRE;
#endif

	SmallBlock * small_block = (SmallBlock*)OS_STRUCT_OFFS((SmallBlock*)p, -1);
	OS_U32 i = small_block->size_slot;
	OS_ASSERT("Heap corrupted!" && i < SMALL_SLOT_COUNT);

#ifdef OS_DEBUG
	{
		int * check_p = (int*)((OS_BYTE*)p + small_block->getDataSize() + OS_SIZEOF_INT);
		OS_ASSERT("Heap corrupted!" && *check_p == DUMMY_SMALL_USED_ID_POST);
		*check_p = DUMMY_SMALL_FREE_ID_POST;
		// OS_MEMSET((int*)p+1, DEAD_BYTE, small_block->getDataSize());

		small_block->removeLink();
	}
#endif

	small_stats.registerFree(small_block->getSize(), small_block->getDataSize());

	((FreeSmallBlock*)small_block)->next = free_small_blocks[i];
	free_small_blocks[i] = (FreeSmallBlock*)small_block;

#if defined(OS_DEBUG)
	// checkMemory();
#endif
}

OS_U32 OSHeapManager::getSizeSmall(void * p)
{
	OS_ASSERT("Trying to size NULL pointer" && p);

#ifdef OS_DEBUG
	p = (OS_BYTE*)p - OS_SIZEOF_INT;
	OS_ASSERT("Heap corrupted!" && *(int*)p == DUMMY_SMALL_USED_ID_PRE);
#endif

	SmallBlock * small_block = (SmallBlock*)OS_STRUCT_OFFS((SmallBlock*)p, -1);

#ifdef OS_DEBUG
	OS_ASSERT("Heap corrupted!" && *(int*)((OS_BYTE*)p + small_block->getDataSize() + OS_SIZEOF_INT) == DUMMY_SMALL_USED_ID_POST);
#endif

	return small_block->getDataSize();
}

inline OS_U32 OSHeapManager::Block::getDataSize() const
{
	OS_ASSERT("Heap corrupted!" && ((size - sizeof(Block) - OS_DUMMY_ID_SIZE) & (ALIGN-1)) == 0);
	return size - sizeof(Block) - OS_DUMMY_ID_SIZE;
}

void OSHeapManager::Block::resetLink()
{
	prev = next = this;
}

void OSHeapManager::Block::removeLink()
{
	prev->next = next;
	next->prev = prev;
}

void OSHeapManager::Block::insertAfter(Block * block)
{
	next = block->next;
	next->prev = this;
	block->next = this;
	prev = block;
}

void OSHeapManager::Block::insertBefore(Block * block)
{
	prev = block->prev;
	prev->next = this;
	block->prev = this;
	next = block;
}

#ifndef OS_USE_HEAP_SAVING_MODE
void OSHeapManager::FreeBlock::resetFreeLink()
{
	prev_free = next_free = this;
}

void OSHeapManager::FreeBlock::removeFreeLink()
{
	prev_free->next_free = next_free;
	next_free->prev_free = prev_free;
}

void OSHeapManager::FreeBlock::insertAfterFreeLink(FreeBlock * block)
{
	next_free = block->next_free;
	next_free->prev_free = this;
	block->next_free = this;
	prev_free = block;
}

void OSHeapManager::FreeBlock::insertBeforeFreeLink(FreeBlock * block)
{
	prev_free = block->prev_free;
	prev_free->next_free = this;
	block->prev_free = this;
	next_free = block;
}

void * OSHeapManager::allocMedium(OS_U32 size OS_DBG_FILEPOS_DECL)
{
#ifdef OS_DEBUG
	// static int step = 0;
	// step++;
#endif

	OS_U32 saveSize = size;
	size = (size + ALIGN - 1 + sizeof(Block) + OS_DUMMY_ID_SIZE) & ~(ALIGN - 1);
	Block * block = dummy_free.next_free;
	if(block->size < size){ // block == &dummy_free => dummy_free.size == 0
		if(size > page_size / 2){
			return allocLarge(saveSize OS_DBG_FILEPOS_PARAM);
		}
		block = (Block*)STD_MALLOC(page_size);
		if(!block){
			return NULL;
		}
#ifdef OS_DEBUG
		block->filename = dbg_filename;
		block->line = dbg_line;
		block->id = cur_id;
#endif
		block->page = next_page++;
		block->size = page_size;
		block->is_free = true;
		block->insertBefore(dummy_block.next);
		((FreeBlock*)block)->insertBeforeFreeLink(dummy_free.next_free);

		medium_stats.alloc_size += block->size;
	}else{
		medium_stats.hit_count++;
#ifdef FIND_BEST_FREE_BLOCK
		for(FreeBlock * next = ((FreeBlock*)block)->next_free; next->size >= size; ){
			block = next;
			next = ((FreeBlock*)block)->next_free;
		}
#endif
	}

	block->size -= size;
	if(block->size < MAX_SMALL_SIZE && (block->size < MAX_SMALL_SIZE/2 || block->next->page != block->page)){
#ifdef OS_DEBUG
		block->filename = dbg_filename;
		block->line = dbg_line;
		block->id = cur_id;
#endif
		block->size += size;
		((FreeBlock*)block)->removeFreeLink();
	}else{
#ifdef OS_DEBUG
		*(int*)(OS_STRUCT_OFFS(block, 1)) = DUMMY_MEDIUM_FREE_ID_PRE;
		*(int*)((OS_BYTE*)(OS_STRUCT_OFFS(block, 1)) + block->getDataSize() + OS_SIZEOF_INT) = DUMMY_MEDIUM_FREE_ID_POST;
#endif

		if(block->size < ((FreeBlock*)block)->next_free->size){
			((FreeBlock*)block)->removeFreeLink();
			insertFreeBlock((FreeBlock*)block);
		}
		OS_ASSERT("Heap corrupted!" && (block->size & (ALIGN-1)) == 0);
		Block * new_block = (Block*)(((OS_BYTE*)block) + block->size);
#ifdef OS_DEBUG
		new_block->filename = dbg_filename;
		new_block->line = dbg_line;
		new_block->id = cur_id;
#endif
		new_block->page = block->page;
		new_block->size = size;
		new_block->insertAfter(block);    
		block = new_block;
	}
	block->is_free = false;

	OS_U32 data_size = block->getDataSize();
	medium_stats.registerAlloc(block->size, data_size);

	OS_BYTE * p = OS_STRUCT_OFFS(block, 1);
	p[-1] = BT_MEDIUM;

#ifdef OS_DEBUG
	*(int*)p = DUMMY_MEDIUM_USED_ID_PRE;
	p += OS_SIZEOF_INT;
	*(int*)(p + data_size) = DUMMY_MEDIUM_USED_ID_POST;
#endif

	OS_ASSERT("Heap align corrupted!" && ((int)(intptr_t)(p) % ALIGN) == 0);
	OS_MEMSET(p, 0, data_size);

#if defined(OS_DEBUG)
	// checkMemory();
#endif

	return p;
}

void OSHeapManager::freeMedium(void * p)
{
	OS_ASSERT("Trying to free NULL pointer" && p);
	OS_ASSERT("Heap corrupted!" && medium_stats.alloc_count > medium_stats.free_count);

#ifdef OS_DEBUG
	p = (OS_BYTE*)p - OS_SIZEOF_INT;
	OS_ASSERT("Heap corrupted!" && *(int*)p == DUMMY_MEDIUM_USED_ID_PRE);
	// *(int*)p = DUMMY_MEDIUM_FREE_ID_PRE;
#endif

	Block * block = (Block*)OS_STRUCT_OFFS((Block*)p, -1);
	OS_ASSERT("Double deallocation!" && !block->is_free);

#ifdef OS_DEBUG
	{
		OS_ASSERT("Heap corrupted!" && *((int*)((OS_BYTE*)p + block->getDataSize() + OS_SIZEOF_INT))	== DUMMY_MEDIUM_USED_ID_POST);
		// *check_p = DUMMY_MEDIUM_FREE_ID_POST;
	}
#endif

	medium_stats.registerFree(block->size, block->getDataSize());

	Block * prev = block->prev;
	if(prev->is_free && prev->page == block->page){
		OS_ASSERT("Heap corrupted!"	&& ((OS_BYTE*)prev) + prev->size == (OS_BYTE*)block);
		prev->size += block->size;

		((FreeBlock*)prev)->removeFreeLink();
		block->removeLink();

		block = prev;
		medium_stats.merge_count++;
	}
	Block * next = block->next;
	if(next->is_free && next->page == block->page){
		OS_ASSERT("Heap corrupted!"
			&& ((OS_BYTE*)block) + block->size == (OS_BYTE*)next);
		block->size += next->size;

		((FreeBlock*)next)->removeFreeLink();
		next->removeLink();

		medium_stats.merge_count++;
	}
	block->is_free = true;

#ifdef OS_DEBUG
	*(int*)(OS_STRUCT_OFFS(block, 1)) = DUMMY_MEDIUM_FREE_ID_PRE;
	*(int*)((OS_BYTE*)(OS_STRUCT_OFFS(block, 1)) + block->getDataSize() + OS_SIZEOF_INT) = DUMMY_MEDIUM_FREE_ID_POST;
	// OS_MEMSET((int*)p+1, DEAD_BYTE, block->getDataSize());
#endif

	insertFreeBlock((FreeBlock*)block);

#if defined(OS_DEBUG)
	// checkMemory();
#endif
}

OS_U32 OSHeapManager::getSizeMedium(void * p)
{
	OS_ASSERT("Trying to size NULL pointer" && p);

#ifdef OS_DEBUG
	p = (OS_BYTE*)p - OS_SIZEOF_INT;
	OS_ASSERT("Heap corrupted!" && *(int*)p == DUMMY_MEDIUM_USED_ID_PRE);
#endif

	Block * block = (Block*)OS_STRUCT_OFFS((Block*)p, -1);

#ifdef OS_DEBUG
	OS_ASSERT("Heap corrupted!" && *(int*)((OS_BYTE*)p + block->getDataSize() + OS_SIZEOF_INT) == DUMMY_MEDIUM_USED_ID_POST);
#endif

	return block->getDataSize();
}

void OSHeapManager::insertFreeBlock(FreeBlock * freeBlock)
{
	for(FreeBlock * cur = dummy_free.next_free; ; cur = cur->next_free){
		OS_ASSERT("Heap corrupted!" && (freeBlock != cur));
		if(freeBlock->size >= cur->size){
			freeBlock->insertBeforeFreeLink(cur);

#ifdef FREE_FREEPAGES
			FreeBlock * next_free;
			if(freeBlock->size == page_size 
				&& cur->size == page_size 
				&& (next_free = cur->next_free)->size == page_size 
				&& (next_free = next_free->next_free)->size == page_size
				&& next_free->next_free->size == page_size
				&& cur->page != cur->next->page
				&& cur->page != cur->prev->page
				)
			{
				medium_stats.alloc_size -= cur->size;
				medium_stats.free_page_count++;

				cur->removeFreeLink();
				cur->removeLink();

				STD_FREE(cur);
			}
#endif

			return;
		}
		OS_ASSERT("Heap corrupted!" && cur != &dummy_free);
		OS_ASSERT("Heap corrupted!" && cur != cur->next_free);
	}
}
#endif // OS_USE_HEAP_SAVING_MODE

void * OSHeapManager::allocLarge(OS_U32 size OS_DBG_FILEPOS_DECL)
{
	size = (size + ALIGN - 1 + sizeof(Block) + OS_DUMMY_ID_SIZE) & ~(ALIGN - 1);
	Block * block = (Block *)STD_MALLOC(size);
	if(!block){
		return NULL;
	}

#ifdef OS_DEBUG
	block->filename = dbg_filename;
	block->line = dbg_line;
	block->id = cur_id;
#endif
	block->page = (OS_U16)-1;
	block->size = size;
	block->is_free = false;
	block->insertBefore(dummy_large_block.next);

	large_stats.alloc_size += size;

	OS_U32 data_size = block->getDataSize();
	large_stats.registerAlloc(block->size, data_size);

	OS_BYTE * p = OS_STRUCT_OFFS(block, 1);
#ifndef OS_USE_HEAP_SAVING_MODE
	p[-1] = BT_LARGE;
#else
	p[-1] &= ~BLOCK_TYPE_MASK;
#endif

#ifdef OS_DEBUG
	*(int*)p = DUMMY_LARGE_USED_ID_PRE;
	p += OS_SIZEOF_INT;
	*(int*)(p + data_size) = DUMMY_LARGE_USED_ID_POST;
#endif

	OS_ASSERT("Heap align corrupted!" && ((int)(intptr_t)(p) % ALIGN) == 0);
	OS_MEMSET(p, 0, data_size);

#if defined(OS_DEBUG)
	// checkMemory();
#endif

	return p;
}

void OSHeapManager::freeLarge(void * p)
{
	OS_ASSERT("Trying to free NULL pointer" && p);
	OS_ASSERT("Heap corrupted!"	&& large_stats.alloc_count > large_stats.free_count);
#ifdef OS_DEBUG
	p = (OS_BYTE*)p - OS_SIZEOF_INT;
	OS_ASSERT("Heap corrupted!" && *(int*)p == DUMMY_LARGE_USED_ID_PRE);
	*(int*)p = DUMMY_LARGE_FREE_ID_PRE;
#endif

	Block * block = (Block*)OS_STRUCT_OFFS((Block*)p, -1);
	OS_ASSERT("Double deallocation!" && !block->is_free);

#ifdef OS_DEBUG
	{
		int * check_p = (int*)((OS_BYTE*)p + block->getDataSize() + OS_SIZEOF_INT);
		OS_ASSERT("Heap corrupted!" && *check_p == DUMMY_LARGE_USED_ID_POST);
		*check_p = DUMMY_LARGE_FREE_ID_POST;
		// OS_MEMSET((int*)p+1, DEAD_BYTE, block->getDataSize());
	}
#endif

	OS_U32 size = block->size;
	large_stats.registerFree(size, block->getDataSize());
	large_stats.alloc_size -= size;

	block->removeLink();

	// OS_MEMSET(block, 0, size);
	STD_FREE(block);
}

OS_U32 OSHeapManager::getSizeLarge(void * p)
{
	OS_ASSERT("Trying to size NULL pointer" && p);

#ifdef OS_DEBUG
	p = (OS_BYTE*)p - OS_SIZEOF_INT;
	OS_ASSERT("Heap corrupted!" && *(int*)p == DUMMY_LARGE_USED_ID_PRE);
#endif

	Block * block = (Block*)OS_STRUCT_OFFS((Block*)p, -1);

#ifdef OS_DEBUG
	OS_ASSERT("Heap corrupted!" && *(int*)((OS_BYTE*)p + block->getDataSize() + OS_SIZEOF_INT) == DUMMY_LARGE_USED_ID_POST);
#endif

	return block->getDataSize();
}

OSHeapManager::OSHeapManager()
{
	OS_ASSERT(ceilPowerOfTwo(OS_HEAP_CHUNK_SIZE) == OS_HEAP_CHUNK_SIZE);

	page_size = ceilPowerOfTwo(DEF_PAGE_SIZE < MAX_SMALL_SIZE*8 ? MAX_SMALL_SIZE*8 : DEF_PAGE_SIZE);
	small_page_size = page_size;

	OS_MEMSET(&small_stats, 0, sizeof(small_stats));
	OS_MEMSET(&large_stats, 0, sizeof(large_stats));

#ifdef OS_DEBUG
	small_stats.min_block_data_size = 0xFFFFFFFF;
	large_stats.min_block_data_size = 0xFFFFFFFF;
#endif

	dummy_small_page.size = 0;
	dummy_small_page.next = NULL;
	small_page = &dummy_small_page;
	OS_MEMSET(free_small_blocks, 0, sizeof(free_small_blocks));
	small_page_offs = 0;
#ifdef OS_DEBUG
	dummy_small_block.resetLink();
	dummy_small_block.size_slot = 0;
	dummy_small_block.filename = "#dummy#";
	dummy_small_block.line = 0;
	cur_id = 0;
#endif

#ifndef OS_USE_HEAP_SAVING_MODE
	medium_size_mask = page_size/2-1;
	OS_MEMSET(&medium_stats, 0, sizeof(medium_stats));

#ifdef OS_DEBUG
	medium_stats.min_block_data_size = 0xFFFFFFFF;
#endif

	dummy_block.resetLink();
	dummy_block.size = 0;
	dummy_block.page = (OS_U16)-1;
	dummy_block.is_free = true;
	// dummy_block.type = BT_MEDIUM;
#ifdef OS_DEBUG
	dummy_block.filename = "#dummy#";
	dummy_block.line = 0;
#endif

	dummy_free.resetLink();
	dummy_free.resetFreeLink();
	dummy_free.size = 0;
	dummy_free.page = (OS_U16)-1;
	dummy_free.is_free = false;
	// dummy_free.type = BT_MEDIUM;
#ifdef OS_DEBUG
	dummy_free.filename = "#dummy#";
	dummy_free.line = 0;
#endif

	next_page = 1;

#endif // OS_USE_HEAP_SAVING_MODE

	dummy_large_block.resetLink();
	dummy_large_block.size = 0;
	dummy_large_block.page = (OS_U16)-1;
	dummy_large_block.is_free = false;
	// dummy_large_block.type = BT_LARGE;
#ifdef OS_DEBUG
	dummy_large_block.filename = "#dummy#";
	dummy_large_block.line = 0;
#endif
}

OSHeapManager::~OSHeapManager()
{
#if defined(_DEBUG) || defined(DEBUG_APP_HEAP_DUMP_LEAK_ON_EXIT)
	const char * dumpFilename = "dump-err-exit.log";
	if(small_stats.alloc_count != small_stats.free_count
#ifndef OS_USE_HEAP_SAVING_MODE
		|| medium_stats.alloc_count != medium_stats.free_count
#endif // OS_USE_HEAP_SAVING_MODE
		|| large_stats.alloc_count != large_stats.free_count
		)
	{
		// dumpUsage(dumpFilename);
		int i = 0;
	}
	else
	{
		// IFILEMGR_Remove(fileMgr, dumpFilename);
	}
#endif

#ifdef _MSC_VER
	OS_ASSERT("Memory leak found!" && small_stats.alloc_count == small_stats.free_count);
#ifndef OS_USE_HEAP_SAVING_MODE
	OS_ASSERT("Memory leak found!" && medium_stats.alloc_count == medium_stats.free_count);
#endif // OS_USE_HEAP_SAVING_MODE
	OS_ASSERT("Memory leak found!" && large_stats.alloc_count == large_stats.free_count);
#endif // _MSC_VER

	// if(small_stats.alloc_count == small_stats.free_count)
	{
		while(small_page != &dummy_small_page){
			SmallPage * cur_page = small_page;
			small_page = small_page->next;
			STD_FREE(cur_page);
		}
	}

	// if(large_stats.alloc_count == large_stats.free_count)
	{
		while(dummy_large_block.next != &dummy_large_block){
			void * p = (char*)(dummy_large_block.next+1) + OS_DUMMY_ID_SIZE/2;
			free(p);
		}
	}

#ifndef OS_USE_HEAP_SAVING_MODE
	// if(medium_stats.alloc_count == medium_stats.free_count)
	{
		for(Block * block = dummy_block.next, * next; block != &dummy_block; block = next){
			next = block->next;
			if(!block->is_free){
				void * p = OS_STRUCT_OFFS(block, 1) + OS_DUMMY_ID_SIZE/2;
				free(p);
			}
		}
		while(dummy_block.next != &dummy_block){
			Block * curBlockPage = dummy_block.next;
			curBlockPage->removeLink();
#ifdef _MSC_VER
			OS_ASSERT("Heap corrupted!" && curBlockPage->page != curBlockPage->next->page);
			OS_ASSERT("Heap corrupted!" && curBlockPage->page != curBlockPage->prev->page);
#endif // _MSC_VER
			STD_FREE(curBlockPage);
		}
	}
#endif // OS_USE_HEAP_SAVING_MODE
}

void * OSHeapManager::malloc(int size OS_DBG_FILEPOS_DECL)
{
#ifdef OS_DEBUG
	cur_id++;
	/* if(cur_id == 1362){
		int i = 0;
	} */
#endif
	if(size <= 0){
		return NULL;
	}

#ifndef USE_STD_MALLOC
#ifndef OS_EMSCRIPTEN // allocSmall doesn't work in emscripten because of heap align error
	if(!(size & ~(MAX_SMALL_SIZE - 1))){
		return allocSmall(size OS_DBG_FILEPOS_PARAM);
	}
#endif

#if defined(OS_DEBUG)
	// checkMemory();
#endif

#ifndef OS_USE_HEAP_SAVING_MODE
	if(!(size & ~medium_size_mask)){
		return allocMedium(size OS_DBG_FILEPOS_PARAM);
	}
#endif

	return allocLarge(size OS_DBG_FILEPOS_PARAM);

#else
	return STD_MALLOC(size);
#endif
}

void OSHeapManager::free(void * p)
{
	if(!p){
		return;
	}

#ifdef OS_DEBUG
	OS_MEMSET(p, DEAD_BYTE, getSize(p));
#endif

#ifndef USE_STD_MALLOC
#ifndef OS_USE_HEAP_SAVING_MODE
	switch(((OS_BYTE*)p)[-1-(int)OS_DUMMY_ID_SIZE/2]){
	case BT_SMALL:
		freeSmall(p);
		break;

	case BT_MEDIUM:
#if defined(OS_DEBUG)
		// checkMemory();
#endif
		freeMedium(p);
		break;

	case BT_LARGE:
#if defined(OS_DEBUG)
		// checkMemory();
#endif
		freeLarge(p);
		break;

	default:
#if defined(OS_DEBUG)
		// checkMemory();
#endif
		OS_ASSERT(false);
	}
#else
	if(((OS_BYTE*)p)[-1-(int)OS_DUMMY_ID_SIZE/2] & BLOCK_TYPE_MASK){
		freeSmall(p);
	}else{
		freeLarge(p);
	}
#endif

#else
	STD_FREE(p);
#endif
}

/*
void * OSHeapManager::realloc(void * p, OS_U32 size OS_DBG_FILEPOS_DECL)
{
	if(!p){
		return malloc(size OS_DBG_FILEPOS_PARAM);
	}
	if(!size)
	{
		free(p);
		return NULL;
	}
#ifdef OS_DEBUG
	void * newData = malloc(size, filename, line);
#else
	void * newData = malloc(size);
#endif
	if(newData)
	{
		OS_U32 oldSize = size(p);
		OS_MEMCPY(newData, p, oldSize < size ? oldSize : size);
		free(p);
		return newData;
	}
	return NULL;
}
*/

void OSHeapManager::setBreakpointId(int id)
{
}

int OSHeapManager::getAllocatedBytes()
{
#ifndef OS_USE_HEAP_SAVING_MODE
	return small_stats.alloc_size + medium_stats.alloc_size + large_stats.alloc_size;
#else // OS_USE_HEAP_SAVING_MODE
	return small_stats.alloc_size + large_stats.alloc_size;
#endif // OS_USE_HEAP_SAVING_MODE
}

int OSHeapManager::getMaxAllocatedBytes()
{
	OS_ASSERT(false);
	return 0;
}

int OSHeapManager::getUsedBytes()
{
#ifndef OS_USE_HEAP_SAVING_MODE
	return small_stats.data_size + medium_stats.data_size + large_stats.data_size;
#else // OS_USE_HEAP_SAVING_MODE
	return small_stats.used_size + large_stats.used_size;
#endif // OS_USE_HEAP_SAVING_MODE
}

int OSHeapManager::getCachedBytes()
{
	return getAllocatedBytes() - getUsedBytes();
}

OS_U32 OSHeapManager::getSize(void * p)
{
	if(!p){
		return 0;
	}

#ifndef OS_USE_HEAP_SAVING_MODE

	switch(((OS_BYTE*)p)[-1-(int)OS_DUMMY_ID_SIZE/2]){
	case BT_SMALL:
		return getSizeSmall(p);

	case BT_MEDIUM:
		return getSizeMedium(p);

	case BT_LARGE:
		return getSizeLarge(p);
	}
	OS_ASSERT(false);
	return 0; // shut up compiler

#else

	if(((OS_BYTE*)p)[-1-(int)OS_DUMMY_ID_SIZE/2] & BLOCK_TYPE_MASK){
		return getSizeSmall(p);
	}
	return getSizeLarge(p);

#endif
}

OS_U32 OSHeapManager::getPageSize() const { return page_size; }

static const char mem_block_type_names[3][7] = {"small", "medium", "large"};

void OSHeapManager::checkMemory()
{
#ifdef OS_DEBUG
	OS_U32 alloc_size, used_size, data_size;

	alloc_size = used_size = data_size = 0;
	for(SmallPage * small_page = this->small_page; small_page; small_page = small_page->next){
		alloc_size += small_page->size;
	}
	OS_ASSERT("Heap corrupted!" && alloc_size == small_stats.alloc_size);

	for(SmallBlock * small_block = dummy_small_block.next;
		small_block != &dummy_small_block; small_block = small_block->next)
	{
		OS_ASSERT("Heap corrupted!" && small_block->next->prev == small_block);
		OS_ASSERT("Heap corrupted!" && small_block->prev->next == small_block);

		OS_U32 block_data_size = small_block->getDataSize();
		OS_ASSERT("Heap corrupted!" && (block_data_size & (ALIGN-1)) == 0);
		OS_ASSERT("Heap corrupted!" && block_data_size <= MAX_SMALL_SIZE);
		OS_ASSERT("Heap corrupted!" && block_data_size >= small_stats.min_block_data_size);
		OS_ASSERT("Heap corrupted!" && block_data_size <= small_stats.max_block_data_size);

		OS_ASSERT("Heap corrupted!" && *(int *)(OS_STRUCT_OFFS(small_block, 1)) == DUMMY_SMALL_USED_ID_PRE);
		OS_ASSERT("Heap corrupted!" && *(int *)((OS_BYTE *)(OS_STRUCT_OFFS(small_block, 1)) + block_data_size + OS_SIZEOF_INT) == DUMMY_SMALL_USED_ID_POST);

		used_size += small_block->getSize();
		data_size += block_data_size;
	}
	OS_ASSERT("Heap corrupted!" && used_size == small_stats.used_size);
	OS_ASSERT("Heap corrupted!" && data_size == small_stats.data_size);

	for(int i = 0; i < SMALL_SLOT_COUNT; i++){
		for(FreeSmallBlock * free_small_block = free_small_blocks[i];
			free_small_block; free_small_block = free_small_block->next)
		{
			SmallBlock * small_block = (SmallBlock*)free_small_block;
			OS_ASSERT("Heap corrupted!" && (small_block->getDataSize() & (ALIGN-1)) == 0);
			OS_ASSERT("Heap corrupted!" && small_block->size_slot == (OS_BYTE)i);
			OS_ASSERT("Heap corrupted!" && *(int *)(OS_STRUCT_OFFS(small_block, 1)) == DUMMY_SMALL_FREE_ID_PRE);
			OS_ASSERT("Heap corrupted!" && *(int *)((OS_BYTE *)(OS_STRUCT_OFFS(small_block, 1)) + small_block->getDataSize() + OS_SIZEOF_INT) == DUMMY_SMALL_FREE_ID_POST);
		}
	}

#ifndef OS_USE_HEAP_SAVING_MODE

	alloc_size = used_size = data_size = 0;
	for(Block * block = dummy_block.next; block != &dummy_block;
		block = block->next)
	{
		OS_ASSERT("Heap corrupted!" && block->is_free == 0 || block->is_free == 1);
		OS_ASSERT("Heap corrupted!" && block->page < next_page);
		OS_ASSERT("Heap corrupted!" && block->next->prev == block);
		OS_ASSERT("Heap corrupted!" && block->prev->next == block);

		OS_U32 block_data_size = block->getDataSize();
		OS_ASSERT("Heap corrupted!" && (block_data_size & (ALIGN-1)) == 0);
		if(!block->is_free){
			OS_ASSERT("Heap corrupted!" && block_data_size >=	medium_stats.min_block_data_size);
			OS_ASSERT("Heap corrupted!" && block_data_size <=	medium_stats.max_block_data_size);
			OS_ASSERT("Heap corrupted!" && *(int *)(OS_STRUCT_OFFS(block, 1)) == DUMMY_MEDIUM_USED_ID_PRE);
			OS_ASSERT("Heap corrupted!" && *(int *)((OS_BYTE *)(OS_STRUCT_OFFS(block, 1)) + block_data_size + OS_SIZEOF_INT) == DUMMY_MEDIUM_USED_ID_POST);

			used_size += block->size;
			data_size += block_data_size;
		}else{
			// OS_ASSERT("Heap corrupted!" && data_size < page_size);
			OS_ASSERT("Heap corrupted!" && *(int *)(OS_STRUCT_OFFS(block, 1)) == DUMMY_MEDIUM_FREE_ID_PRE);
			OS_ASSERT("Heap corrupted!" && *(int *)((OS_BYTE *)(OS_STRUCT_OFFS(block, 1)) + block_data_size + OS_SIZEOF_INT) == DUMMY_MEDIUM_FREE_ID_POST);
		}
		alloc_size += block->size;
	}
	OS_ASSERT("Heap corrupted!" && alloc_size == medium_stats.alloc_size);
	OS_ASSERT("Heap corrupted!" && used_size == medium_stats.used_size);
	OS_ASSERT("Heap corrupted!" && data_size == medium_stats.data_size);

	for(FreeBlock * block = dummy_free.next_free; block != &dummy_free;
		block = block->next_free)
	{
		OS_ASSERT("Heap corrupted!" && block->is_free == 1);
		OS_ASSERT("Heap corrupted!" && block->page < next_page);
		OS_ASSERT("Heap corrupted!" && block->next_free->prev_free == block);
		OS_ASSERT("Heap corrupted!" && block->prev_free->next_free == block);
	}
#endif // OS_USE_HEAP_SAVING_MODE

	alloc_size = used_size = data_size = 0;

	for(Block * large_block = dummy_large_block.next;
		large_block != &dummy_large_block; large_block = large_block->next)
	{
		OS_ASSERT("Heap corrupted!" && large_block->next->prev == large_block);
		OS_ASSERT("Heap corrupted!" && large_block->prev->next == large_block);

		OS_U32 block_data_size = large_block->getDataSize();
		OS_ASSERT("Heap corrupted!" && block_data_size >= large_stats.min_block_data_size);
		OS_ASSERT("Heap corrupted!" && block_data_size <=	large_stats.max_block_data_size);

		alloc_size += large_block->size;
		used_size += large_block->size;
		data_size += block_data_size;
	}
	OS_ASSERT("Heap corrupted!" && alloc_size == large_stats.alloc_size);
	OS_ASSERT("Heap corrupted!" && used_size == large_stats.used_size);
	OS_ASSERT("Heap corrupted!" && data_size == large_stats.data_size);
#endif // OS_DEBUG
}

void OSHeapManager::writeFile(OS * os, OS::FileHandle * f, const char * buf)
{
	int len = (int)OS_STRLEN(buf);
	os->writeFile(buf, len, f);
}

void OSHeapManager::writeStats(OS * os, OS::FileHandle * f)
{
	char buf[256];

	OS_SNPRINTF(buf, sizeof(buf)-1, "PAGE SIZE:\t%d\n\n", getPageSize());
	writeFile(os, f, buf);

#ifndef OS_USE_HEAP_SAVING_MODE
#ifdef OS_DEBUG
	const char * header_str = "TYPE\tCOUNT\talloc\tfree\thit\tcur\tSIZE\tavg\talloc\tused\tdata\tused max\tdata max\tmin block\tmax block\n";
#else
	const char * header_str = "Type\tCOUNT\talloc\tfree\thit\tcur\tSIZE\tavg\talloc\tused\tdata\n";
#endif
#else // OS_USE_HEAP_SAVING_MODE
#ifdef OS_DEBUG
	const char * header_str = "TYPE\tCOUNT\talloc\tfree\tcur\tSIZE\tavg\talloc\tused\tdata\tused max\tdata max\tmin block\tmax block\n";
#else
	const char * header_str = "Type\tCOUNT\talloc\tfree\tcur\tSIZE\tavg\talloc\tused\tdata\n";
#endif
#endif // OS_USE_HEAP_SAVING_MODE
	writeFile(os, f, header_str);

	Stats stats[3];
	getStats(stats[0], stats[1], stats[2]);
	for(int i = 0; i < 3; i++)
	{
		if(!stats[i].alloc_size)
			continue;

		OS_U32 cur_count = stats[i].alloc_count - stats[i].free_count;
		OS_U32 cur_avg_data_size = cur_count ? stats[i].data_size / cur_count : 0;
#ifndef OS_USE_HEAP_SAVING_MODE
#ifdef OS_DEBUG
		OS_SNPRINTF(buf, sizeof(buf)-1, "%s\t\t%d\t%d\t%d\t%d\t\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", mem_block_type_names[i], 
			stats[i].alloc_count, stats[i].free_count, stats[i].hit_count, cur_count, cur_avg_data_size,
			stats[i].alloc_size, stats[i].used_size, stats[i].data_size, stats[i].max_used_size, stats[i].max_data_size,
			stats[i].min_block_data_size, stats[i].max_block_data_size);
#else
		OS_SNPRINTF(buf, sizeof(buf)-1, "%s\t\t%d\t%d\t%d\t%d\t\t%d\t%d\t%d\t%d\n", mem_block_type_names[i], 
			stats[i].alloc_count, stats[i].free_count, stats[i].hit_count, cur_count, cur_avg_data_size,
			stats[i].alloc_size, stats[i].used_size, stats[i].data_size);
#endif
#else // OS_USE_HEAP_SAVING_MODE
#ifdef OS_DEBUG
		OS_SNPRINTF(buf, sizeof(buf)-1, "%s\t\t%d\t%d\t%d\t\t%d\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n", mem_block_type_names[i], 
			stats[i].alloc_count, stats[i].free_count, cur_count, cur_avg_data_size,
			stats[i].alloc_size, stats[i].used_size, stats[i].data_size, stats[i].max_used_size, stats[i].max_data_size,
			stats[i].min_block_data_size, stats[i].max_block_data_size);
#else
		OS_SNPRINTF(buf, sizeof(buf)-1, "%s\t\t%d\t%d\t%d\t\t%d\t%d\t%d\t%d\n", mem_block_type_names[i], 
			stats[i].alloc_count, stats[i].free_count, cur_count, cur_avg_data_size,
			stats[i].alloc_size, stats[i].used_size, stats[i].data_size);
#endif
#endif // OS_USE_HEAP_SAVING_MODE
		writeFile(os, f, buf);
	}

	SummaryStats summary_stats;
	getStats(summary_stats);
	OS_U32 cur_count = summary_stats.alloc_count - summary_stats.free_count;
#ifndef OS_USE_HEAP_SAVING_MODE
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "SUMMARY\t\t%d\t%d\t%d\t%d\t\t\t%d\t%d\t%d\t%d\t%d\n\n", 
		summary_stats.alloc_count, summary_stats.free_count, summary_stats.hit_count, cur_count, 
		summary_stats.alloc_size, summary_stats.used_size, summary_stats.data_size, 
		summary_stats.max_used_size, summary_stats.max_data_size);
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "SUMMARY\t\t%d\t%d\t%d\t%d\t\t\t%d\t%d\t%d\n\n", 
		summary_stats.alloc_count, summary_stats.free_count, summary_stats.hit_count, cur_count, 
		summary_stats.alloc_size, summary_stats.used_size, summary_stats.data_size);
#endif
#else // OS_USE_HEAP_SAVING_MODE
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "SUMMARY\t\t%d\t%d\t%d\t\t\t%d\t%d\t%d\t%d\t%d\n\n", 
		summary_stats.alloc_count, summary_stats.free_count, cur_count, 
		summary_stats.alloc_size, summary_stats.used_size, summary_stats.data_size, 
		summary_stats.max_used_size, summary_stats.max_data_size);
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "SUMMARY\t\t%d\t%d\t%d\t\t\t%d\t%d\t%d\n\n", 
		summary_stats.alloc_count, summary_stats.free_count, cur_count, 
		summary_stats.alloc_size, summary_stats.used_size, summary_stats.data_size);
#endif
#endif // OS_USE_HEAP_SAVING_MODE
	writeFile(os, f, buf);
}

// ==============================================================================================

#ifdef OS_DEBUG
void OSHeapManager::writeSmallBlockHeader(OS * os, OS::FileHandle * f)
{
	char buf[256];
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "== BLOCKS TYPE: %s ===============================================================\nSIZE\tID\tLINE\tFILENAME\n", mem_block_type_names[0]);
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "== BLOCKS TYPE: %s ===============================================================\nSIZE\tLINE\tFILENAME\n", mem_block_type_names[0]);
#endif
	writeFile(os, f, buf);
}

void OSHeapManager::writeSmallBlock(OS * os, OS::FileHandle * f, SmallBlock * block)
{
	char buf[256];
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "%d\t%d\t%d\t%s\n", block->getDataSize(), block->id, block->line, block->filename);
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "%d\t%d\t%s\n", block->getDataSize(), block->line, block->filename);
#endif
	writeFile(os, f, buf);
}

void OSHeapManager::writeSmallBlocks(OS * os, OS::FileHandle * f)
{
	SmallBlock * block = dummy_small_block.next;
	for(; block != &dummy_small_block; block = block->next){
		writeSmallBlock(os, f, block);
	}
	writeFile(os, f, "\n");
}
#endif

// ==============================================================================================

void OSHeapManager::writeBlockHeader(OS * os, OS::FileHandle * f, int type)
{
	char buf[256];
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "== BLOCKS TYPE: %s ===============================================================\nSIZE\tID\tPAGE\tLINE\tFILENAME\n", mem_block_type_names[type]);
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "== BLOCKS TYPE: %s ===============================================================\nSIZE\tPAGE\n", mem_block_type_names[type]);
#endif
	writeFile(os, f, buf);
}

void OSHeapManager::writeBlock(OS * os, OS::FileHandle * f, Block * block)
{
	char buf[256];
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "%d\t%d\t%d\t%d\t%s\n", block->getDataSize(), block->id, block->page, block->line, block->filename);
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "%d\t%d\n", block->getDataSize(), block->page);
#endif
	writeFile(os, f, buf);
}

void OSHeapManager::writeBlocks(OS * os, OS::FileHandle * f, Block * dummy_block)
{
	Block * block = dummy_block->next;
	for(; block != dummy_block; block = block->next){
		if(!block->is_free){
			writeBlock(os, f, block);
		}
	}
	writeFile(os, f, "\n");
}

// ==============================================================================================

#ifndef OS_USE_HEAP_SAVING_MODE

void OSHeapManager::writeFreeBlockHeader(OS * os, OS::FileHandle * f)
{
	char buf[256];
#ifdef OS_DEBUG
	OS_SNPRINTF(buf, sizeof(buf)-1, "== STD_FREE BLOCKS ===============================================================\nSIZE\tPAGE\tLINE\tFILENAME\n");
#else
	OS_SNPRINTF(buf, sizeof(buf)-1, "== STD_FREE BLOCKS ===============================================================\nSIZE\tPAGE\n");
#endif
	writeFile(os, f, buf);
}

void OSHeapManager::writeFreeBlocks(OS * os, OS::FileHandle * f)
{
	FreeBlock * block = dummy_free.next_free;
	for(; block != &dummy_free; block = block->next_free){
		writeBlock(os, f, block);
	}
	writeFile(os, f, "\n");
}

#endif // OS_USE_HEAP_SAVING_MODE

void OSHeapManager::dumpUsage(OS * os, const OS_CHAR * filename)
{
	OS::FileHandle * f = os->openFile(filename, "wt");
	if(!f){
		return;
	}
	OS_U32 free_size = 0x7fffffff;
	writeStats(os, f);

#ifndef OS_USE_HEAP_SAVING_MODE
	writeFreeBlockHeader(os, f);
	writeFreeBlocks(os, f);
#endif // OS_USE_HEAP_SAVING_MODE

#ifdef OS_DEBUG
	writeSmallBlockHeader(os, f);
	writeSmallBlocks(os, f);
#endif

#ifndef OS_USE_HEAP_SAVING_MODE
	writeBlockHeader(os, f, 1);
	writeBlocks(os, f, &dummy_block);
#endif // OS_USE_HEAP_SAVING_MODE

	writeBlockHeader(os, f, 2);
	writeBlocks(os, f, &dummy_large_block);

	os->closeFile(f);
}

void OSHeapManager::getStats(Stats& small_stats, Stats& medium_stats, Stats& large_stats)
{
	*(SimpleStats*)&small_stats = this->small_stats;
	small_stats.merge_count = 0;

#ifndef OS_USE_HEAP_SAVING_MODE
	medium_stats = this->medium_stats;
#else
	OS_MEMSET(&medium_stats, 0, sizeof(medium_stats));
#endif

	*(SimpleStats*)&large_stats = this->large_stats;
	large_stats.merge_count = 0;

#ifdef OS_DEBUG
	if(small_stats.min_block_data_size > small_stats.max_block_data_size){
		small_stats.min_block_data_size = small_stats.max_block_data_size = 0;
	}
	if(medium_stats.min_block_data_size > medium_stats.max_block_data_size){
		medium_stats.min_block_data_size = medium_stats.max_block_data_size = 0;
	}
	if(large_stats.min_block_data_size > large_stats.max_block_data_size){
		large_stats.min_block_data_size = large_stats.max_block_data_size = 0;
	}
#endif
}

void OSHeapManager::getStats(SummaryStats& stats)
{
#ifndef OS_USE_HEAP_SAVING_MODE
	stats.alloc_count = small_stats.alloc_count + medium_stats.alloc_count + large_stats.alloc_count;
	stats.alloc_size = small_stats.alloc_size + medium_stats.alloc_size + large_stats.alloc_size;
	stats.used_size = small_stats.used_size + medium_stats.used_size + large_stats.used_size;
	stats.data_size = small_stats.data_size + medium_stats.data_size + large_stats.data_size;
	stats.free_count = small_stats.free_count + medium_stats.free_count + large_stats.free_count;
	stats.hit_count = small_stats.hit_count + medium_stats.hit_count + large_stats.hit_count;
	stats.merge_count = medium_stats.merge_count;

#ifdef OS_DEBUG
	stats.max_used_size = small_stats.max_used_size + medium_stats.max_used_size + large_stats.max_used_size;
	stats.max_data_size = small_stats.max_data_size + medium_stats.max_data_size + large_stats.max_data_size;

	if(small_stats.min_block_data_size > small_stats.max_block_data_size){
		stats.min_small_block_data_size = stats.max_small_block_data_size = 0;
	}else{
		stats.min_small_block_data_size = small_stats.min_block_data_size;
		stats.max_small_block_data_size = small_stats.max_block_data_size;
	}

	if(medium_stats.min_block_data_size > medium_stats.max_block_data_size){
		stats.min_medium_block_data_size = stats.max_medium_block_data_size = 0;
	}else{
		stats.min_medium_block_data_size = medium_stats.min_block_data_size;
		stats.max_medium_block_data_size = medium_stats.max_block_data_size;
	}

	if(large_stats.min_block_data_size > large_stats.max_block_data_size){
		stats.min_large_block_data_size = stats.max_large_block_data_size = 0;
	}else{
		stats.min_large_block_data_size = large_stats.min_block_data_size;
		stats.max_large_block_data_size = large_stats.max_block_data_size;
	}
#endif

#else // OS_USE_HEAP_SAVING_MODE

	stats.alloc_count = small_stats.alloc_count + large_stats.alloc_count;
	stats.alloc_size = small_stats.alloc_size + large_stats.alloc_size;
	stats.used_size = small_stats.used_size + large_stats.used_size;
	stats.data_size = small_stats.data_size + large_stats.data_size;
	stats.free_count = small_stats.free_count + large_stats.free_count;
	stats.hit_count = small_stats.hit_count + large_stats.hit_count;
	stats.merge_count = 0;

#ifdef OS_DEBUG
	stats.max_used_size = small_stats.max_used_size + large_stats.max_used_size;
	stats.max_data_size = small_stats.max_data_size + large_stats.max_data_size;

	if(small_stats.min_block_data_size > small_stats.max_block_data_size){
		stats.min_small_block_data_size = stats.max_small_block_data_size = 0;
	}else{
		stats.min_small_block_data_size = small_stats.min_block_data_size;
		stats.max_small_block_data_size = small_stats.max_block_data_size;
	}

	if(large_stats.min_block_data_size > large_stats.max_block_data_size){
		stats.min_large_block_data_size = stats.max_large_block_data_size = 0;
	}else{
		stats.min_large_block_data_size = large_stats.min_block_data_size;
		stats.max_large_block_data_size = large_stats.max_block_data_size;
	}
#endif

#endif // OS_USE_HEAP_SAVING_MODE
}
